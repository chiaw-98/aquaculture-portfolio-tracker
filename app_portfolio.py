import re
import streamlit as st
import pandas as pd
import altair as alt

st.set_page_config(layout="wide", page_title="Aquaculture Species Tracker | Animal Welfare")

# ==============================================================================
# GOOGLE SHEET URLs
# Each tab must be published as CSV via File → Share → Publish to web
# ==============================================================================

SPECIES_COUNTRY_DETAIL_URL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vS6f3TQ1C0Jbx2h36HnWI28eVVh-7ThdpdqGvKIhKkn1KtQlZDR3cLQv5DmArVvQA/pub?gid=1207069294&single=true&output=csv"
ENS_URL                    = "https://docs.google.com/spreadsheets/d/e/2PACX-1vS6f3TQ1C0Jbx2h36HnWI28eVVh-7ThdpdqGvKIhKkn1KtQlZDR3cLQv5DmArVvQA/pub?gid=511623742&single=true&output=csv"
JACCARD_URL                = "https://docs.google.com/spreadsheets/d/e/2PACX-1vS6f3TQ1C0Jbx2h36HnWI28eVVh-7ThdpdqGvKIhKkn1KtQlZDR3cLQv5DmArVvQA/pub?gid=2123136918&single=true&output=csv"
SHEET_URL                  = "https://docs.google.com/spreadsheets/d/10HE5wznbTTdQc5eDc-JPFzkLi7dQJXnx/edit?usp=sharing&ouid=113734724837871302859&rtpof=true&sd=true"

# ==============================================================================
# DATA LOADING
# ==============================================================================

@st.cache_data(ttl=3600)
def load_data():
    detail  = pd.read_csv(SPECIES_COUNTRY_DETAIL_URL)
    ens     = pd.read_csv(ENS_URL)
    jaccard = pd.read_csv(JACCARD_URL)
    return detail, ens, jaccard


# ==============================================================================
# SHARED HELPERS
# ==============================================================================

def format_individuals(val):
    if pd.isna(val) or val == 0:
        return ""
    if val >= 1e9:
        return f"{val / 1e9:.1f}B"
    if val >= 1e6:
        return f"{val / 1e6:.0f}M"
    if val >= 1e3:
        return f"{val / 1e3:.0f}K"
    return f"{val:.0f}"


def get_share_cols(detail):
    cols = [c for c in detail.columns if re.match(r"share_of_group_\d{4}$", c)]
    return sorted(cols)


def get_years(share_cols):
    return [int(c.split("_")[-1]) for c in share_cols]


# Shared methodology text used in both Species Watchlist and Country Breakdown expanders
_METHODOLOGY_MD = """
**Rel. CAGR (%/yr)** is a *rate* measure: how fast this species is gaining or losing share \
per year, relative to the taxon-group sector overall.

> Species CAGR = (Individuals 2023 / Individuals 2019)^(1/4) - 1
>
> Group CAGR = (Group total 2023 / Group total 2019)^(1/4) - 1
>
> **Relative CAGR = Species CAGR - Group CAGR**

Positive = outpacing the sector. Negative = losing ground. \
Computed for Stable species with consistent reporting only.

---

**Share Δ (pp)** is a *stock* measure: how much this species' share of the taxon-group \
total has shifted from 2019 to 2023, in percentage points.

> **Share Δ = (Species 2023 / Group 2023 × 100) - (Species 2019 / Group 2019 × 100)**

---

Reading both together gives a fuller picture: Rel. CAGR shows the pace of change, \
Share Δ shows the magnitude.

Individual counts are estimated from FAO tonnage using species-specific conversion \
factors (individuals per tonne). Mid-point of the lower/upper bound range is used throughout.
"""


# ==============================================================================
# PAGE 1 — SPECIES WATCHLIST
# ==============================================================================

def compute_species_summary(detail):
    stable = detail[(detail["status"] == "Stable") & (~detail["is_volatile"])].copy()
    if stable.empty:
        return pd.DataFrame()

    def agg_species(g):
        total_mid   = g["indiv_mid_2023"].sum()
        valid_rcagr = g.dropna(subset=["relative_CAGR_simple"])
        valid_sc    = g.dropna(subset=["share_change_pp"])
        rcagr_denom = valid_rcagr["indiv_mid_2023"].sum()
        sc_denom    = valid_sc["indiv_mid_2023"].sum()
        return pd.Series({
            "total_indiv_mid_end":       total_mid,
            "total_indiv_lower_end":     g["indiv_lower_2023"].sum(),
            "total_indiv_upper_end":     g["indiv_upper_2023"].sum(),
            "n_countries_present":       (g["indiv_mid_2023"] > 0).sum(),
            "n_countries_outpacing":     (valid_rcagr["relative_CAGR_simple"] > 0).sum(),
            "n_countries_losing_ground": (valid_rcagr["relative_CAGR_simple"] < 0).sum(),
            "weighted_relative_CAGR_simple": (
                (valid_rcagr["relative_CAGR_simple"] * valid_rcagr["indiv_mid_2023"]).sum()
                / rcagr_denom
            ) if rcagr_denom > 0 else None,
            "weighted_share_change_pp": (
                (valid_sc["share_change_pp"] * valid_sc["indiv_mid_2023"]).sum()
                / sc_denom
            ) if sc_denom > 0 else None,
        })

    summary = (
        stable
        .groupby(["SPECIES.ALPHA_3_CODE", "Species_name", "Scientific_Name", "taxon_group"])
        .apply(agg_species, include_groups=False)
        .reset_index()
    )
    summary["multi_country_outpacing"] = summary["n_countries_outpacing"] >= 2
    return summary


def build_watchlist_display(summary):
    return pd.DataFrame({
        "Species":                  summary["Species_name"].values,
        "Scientific name":          summary["Scientific_Name"].values,
        "Group":                    summary["taxon_group"].values,
        "Animals farmed (2023)":    [format_individuals(v) for v in summary["total_indiv_mid_end"]],
        "Rel. CAGR (%/yr)":         summary["weighted_relative_CAGR_simple"].round(1).values,
        "Share Δ (pp)":             summary["weighted_share_change_pp"].round(1).values,
        "Countries outpacing":      summary["n_countries_outpacing"].astype(int).values,
        "Countries losing ground":  summary["n_countries_losing_ground"].astype(int).values,
    })


WATCHLIST_COL_CONFIG = {
    "Species": st.column_config.TextColumn("Species"),
    "Scientific name": st.column_config.TextColumn("Scientific name"),
    "Group": st.column_config.TextColumn("Group"),
    "Animals farmed (2023)": st.column_config.TextColumn(
        "Animals farmed (2023)",
        help="Mid-point estimate of individuals farmed in 2023.",
    ),
    "Rel. CAGR (%/yr)": st.column_config.NumberColumn(
        "Rel. CAGR (%/yr)",
        help=(
            "How fast this species is growing relative to aquaculture as a whole.\n\n"
            "Positive = gaining sector share. Negative = losing ground.\n\n"
            "Weighted average across countries by 2023 individual count. "
            "Species CAGR minus sector CAGR."
        ),
        format="%.1f",
    ),
    "Share Δ (pp)": st.column_config.NumberColumn(
        "Share Δ (pp)",
        help=(
            "How much this species' share of total farmed animals has shifted, in percentage points.\n\n"
            "(Species % of group at end year) - (Species % of group at start year), "
            "weighted across countries by 2023 individual count."
        ),
        format="%.1f",
    ),
    "Countries outpacing": st.column_config.NumberColumn(
        "Countries outpacing",
        help="Number of countries where this species is growing faster than that country's sector overall.",
    ),
    "Countries losing ground": st.column_config.NumberColumn(
        "Countries losing ground",
        help="Number of countries where this species is growing more slowly than the sector.",
    ),
}


def render_country_expanders(summary, filtered, section):
    for _, row in summary.iterrows():
        species_data = filtered[filtered["Species_name"] == row["Species_name"]].copy()
        if section == "gaining":
            breakdown = species_data[
                species_data["relative_CAGR_simple"].notna() &
                (species_data["relative_CAGR_simple"] > 0)
            ][["Country_name", "relative_CAGR_simple", "indiv_mid_2023"]].sort_values(
                "relative_CAGR_simple", ascending=False
            )
            n     = len(breakdown)
            label = f"{row['Species_name']}: {n} {'country' if n == 1 else 'countries'} outpacing"
        else:
            breakdown = species_data[
                species_data["relative_CAGR_simple"].notna() &
                (species_data["relative_CAGR_simple"] <= 0)
            ][["Country_name", "relative_CAGR_simple", "indiv_mid_2023"]].sort_values(
                "relative_CAGR_simple", ascending=True
            )
            n     = len(breakdown)
            label = f"{row['Species_name']}: {n} {'country' if n == 1 else 'countries'} losing ground"

        with st.expander(label):
            if breakdown.empty:
                st.write("No country-level data available.")
            else:
                breakdown.columns = ["Country", "Rel. CAGR (%/yr)", "Animals farmed (2023)"]
                breakdown["Animals farmed (2023)"] = (
                    breakdown["Animals farmed (2023)"].apply(format_individuals)
                )
                st.dataframe(breakdown, use_container_width=True, hide_index=True)


def page_watchlist(detail):
    st.caption("📋  SPECIES WATCHLIST")
    st.title("Which aquaculture species are expanding, and which are contracting?")
    st.markdown("""
Each species gaining ground in aquaculture is a potential **new target population** that will
likely require independent welfare research and intervention infrastructure before any animal in it
can be meaningfully helped. Species losing ground signal potentially diminishing returns.
""")

    with st.expander("📐  How these numbers are calculated", expanded=False):
        st.markdown(_METHODOLOGY_MD)

    all_countries = sorted(detail["Country_name"].unique())
    n_all = len(all_countries)
    with st.sidebar.expander(f"Select countries ({n_all} of {n_all} selected)", expanded=False):
        selected_countries = st.multiselect(
            "Countries", options=all_countries, default=all_countries,
            label_visibility="collapsed",
        )

    taxon_filter  = st.sidebar.radio("Taxon group", ["All", "Finfish", "Crustacean"])
    min_countries = st.sidebar.slider("Minimum countries with data", 1, 20, 1)
    multi_only    = st.sidebar.checkbox("Multi-country signal only", value=False)
    min_scale_m   = st.sidebar.slider("Minimum scale (millions of individuals)", 0, 1000, 1000)

    filtered = detail[detail["Country_name"].isin(selected_countries)]
    if taxon_filter != "All":
        filtered = filtered[filtered["taxon_group"] == taxon_filter]

    summary = compute_species_summary(filtered)
    if summary.empty:
        st.warning("No species match the current filters.")
        return

    summary = summary[summary["weighted_relative_CAGR_simple"].notna()]
    summary = summary[summary["n_countries_present"] >= min_countries]
    if multi_only:
        summary = summary[summary["multi_country_outpacing"]]
    if min_scale_m > 0:
        summary = summary[summary["total_indiv_mid_end"] >= min_scale_m * 1_000_000]

    if summary.empty:
        st.warning("No species match the current filters.")
        return

    gaining = summary[summary["weighted_relative_CAGR_simple"] > 0].sort_values(
        "weighted_share_change_pp", ascending=False)
    losing  = summary[summary["weighted_relative_CAGR_simple"] <= 0].sort_values(
        "weighted_share_change_pp", ascending=True)

    if min_scale_m > 0:
        st.caption(
            f"Showing species with {format_individuals(min_scale_m * 1_000_000)}+ individuals. "
            "Adjust 'Minimum scale' in the sidebar to show smaller species."
        )

    st.markdown("### 📈 Gaining ground on the sector")
    if gaining.empty:
        st.write("No species gaining ground under current filters.")
    else:
        st.dataframe(build_watchlist_display(gaining), use_container_width=True,
                     hide_index=True, column_config=WATCHLIST_COL_CONFIG)

    st.divider()

    st.markdown("### 📉 Losing ground")
    if losing.empty:
        st.write("No species losing ground under current filters.")
    else:
        st.dataframe(build_watchlist_display(losing), use_container_width=True,
                     hide_index=True, column_config=WATCHLIST_COL_CONFIG)

    st.divider()

    st.markdown("### 🌍 Country-level breakdown")
    tab_gaining, tab_losing = st.tabs(["📈 Gaining species", "📉 Losing species"])
    with tab_gaining:
        if gaining.empty:
            st.write("No gaining species under current filters.")
        else:
            render_country_expanders(gaining, filtered, "gaining")
    with tab_losing:
        if losing.empty:
            st.write("No losing species under current filters.")
        else:
            render_country_expanders(losing, filtered, "losing")

    st.divider()
    st.info(
        "**Only species with consistent reporting across the full data period are ranked.** "
        "Species with erratic year-to-year data are included in total animal estimates but "
        "excluded from growth rankings.",
        icon="ℹ️",
    )


# ==============================================================================
# PAGE 2 — COUNTRY BREAKDOWN
# ==============================================================================

TRAJECTORY_COLOURS = {
    "DIVERSIFYING":          "#2e7d32",
    "STABLE":                "#1565c0",
    "CONSOLIDATING":         "#c62828",
    "UNCERTAIN TRAJECTORY":  "#546e7a",
    "No finfish farmed":     "#90a4ae",
    "No crustaceans farmed": "#90a4ae",
}

TRAJECTORY_ICONS = {
    "DIVERSIFYING":          "↗",
    "STABLE":                "→",
    "CONSOLIDATING":         "↘",
    "UNCERTAIN TRAJECTORY":  "?",
    "No finfish farmed":     "",
    "No crustaceans farmed": "",
}

STATUS_SYMBOL = {
    "Newcomer": "↑",
    "Exiting":  "↓",
    "Blip":     "~",
}


def render_ens_badge(ens_row):
    traj       = ens_row["consensus_trajectory"]
    colour     = TRAJECTORY_COLOURS.get(traj, "#546e7a")
    icon       = TRAJECTORY_ICONS.get(traj, "")
    agree      = ens_row.get("scenarios_agree", True)
    low_volume = ens_row.get("low_volume", False)

    st.markdown(
        f'<span style="background:{colour};color:white;padding:4px 12px;'
        f'border-radius:4px;font-weight:600;font-size:0.95em">'
        f'{icon} {traj}</span>',
        unsafe_allow_html=True,
    )

    if low_volume:
        st.caption("Low production volume: trajectory less certain.")

    if not agree:
        with st.expander("ⓘ  More info", expanded=False):
            st.markdown(
                "**Scenario sensitivity:** the classification varies depending on how "
                "unidentified species (NEI) tonnage is allocated:"
            )
            rows = {
                "Conservative": ens_row.get("trajectory_conservative", "n/a"),
                "Medium":       ens_row.get("trajectory_medium",       "n/a"),
                "Maximum":      ens_row.get("trajectory_maximum",      "n/a"),
            }
            st.table(pd.DataFrame.from_dict(rows, orient="index",
                                             columns=["Classification"]))
            st.caption(
                "Where scenarios agree on direction the consensus label is shown. "
                "Where they disagree on direction the trajectory is marked Uncertain."
            )


def render_share_heatmap(taxon_data, share_cols, years):
    if taxon_data.empty:
        return

    display = taxon_data[
        ["Species_name", "status", "is_volatile"] + share_cols +
        ["relative_CAGR_simple", "share_change_pp", "indiv_mid_2023"]
    ].copy()

    year_strs  = [str(y) for y in years]
    rename_map = {sc: str(int(sc.split("_")[-1])) for sc in share_cols}
    display    = display.rename(columns=rename_map)

    for col in year_strs:
        if col in display.columns:
            # Replace exact zero (truly no production that year) with NaN so it
            # renders as "—" rather than "0.0%" which implies a tiny rounding artefact
            display[col] = display[col].where(display[col] != 0.0)
            display[col] = (display[col] * 100).round(1)

    def status_symbol(row):
        if row["is_volatile"]:
            return "⚡"
        return STATUS_SYMBOL.get(row["status"], "")

    display.insert(0, "Flag", display.apply(status_symbol, axis=1))

    last_yr = year_strs[-1]
    if last_yr in display.columns:
        display = display.sort_values(last_yr, ascending=False, na_position="last")

    non_stable = (display["status"] != "Stable") | display["is_volatile"]

    valid_year_cols = [c for c in year_strs if c in display.columns]
    disp_cols = ["Flag", "Species_name"] + valid_year_cols + [
        "relative_CAGR_simple", "share_change_pp", "indiv_mid_2023"
    ]
    out = display[[c for c in disp_cols if c in display.columns]].copy()
    out["indiv_mid_2023"] = out["indiv_mid_2023"].apply(format_individuals)

    def highlight_non_stable(col):
        return [
            "color: darkorange; font-style: italic" if non_stable.iloc[i] else ""
            for i in range(len(col))
        ]

    max_val = out[valid_year_cols].max().max() if valid_year_cols else 1

    styler = (
        out.style
        .background_gradient(subset=valid_year_cols, cmap="Blues",
                             vmin=0, vmax=max_val if max_val > 0 else 1)
        .apply(highlight_non_stable, subset=["Species_name"])
        .format({c: "{:.1f}%" for c in valid_year_cols}, na_rep="—")
        .format({"relative_CAGR_simple": "{:.1f}", "share_change_pp": "{:.1f}"}, na_rep="—")
    )

    st.dataframe(
        styler,
        use_container_width=True,
        hide_index=True,
        column_config={
            "Flag": st.column_config.TextColumn("Flag", width="small"),
            "Species_name": st.column_config.TextColumn("Species"),
            **{yr: st.column_config.NumberColumn(
                yr,
                help=f"Share of {yr} taxon-group total (%)",
                format="%.1f%%",
            ) for yr in valid_year_cols},
            "relative_CAGR_simple": st.column_config.NumberColumn(
                "Rel. CAGR (%/yr)",
                help="Species CAGR minus country sector CAGR. Stable species only.",
                format="%.1f",
            ),
            "share_change_pp": st.column_config.NumberColumn(
                "Share Δ (pp)",
                help="Change in share of group from 2019 to 2023, percentage points. Stable species only.",
                format="%.1f",
            ),
            "indiv_mid_2023": st.column_config.TextColumn(
                "Animals farmed (2023)",
                help="Mid-point estimate of individuals farmed in 2023.",
            ),
        },
    )

    st.caption(
        "**Flag:** ⚡ volatile  ·  ↑ newcomer  ·  ↓ exiting  ·  ~ blip  ·  "
        "Orange italic = excluded from growth rankings  ·  "
        "Share % = proportion of this country's taxon-group total that year  ·  "
        "— = no production recorded that year"
    )

    with st.expander("📐  How these numbers are calculated", expanded=False):
        st.markdown(_METHODOLOGY_MD)


def render_bar_chart(taxon_data, country, taxon):
    chart_data = taxon_data[
        ["Species_name", "indiv_mid_2023", "relative_CAGR_simple"]
    ].dropna(subset=["indiv_mid_2023"]).copy()

    chart_data["Trajectory"] = chart_data["relative_CAGR_simple"].apply(
        lambda x: "Gaining ground" if (pd.notna(x) and x > 0) else "Losing ground"
    )

    if chart_data.empty:
        return

    chart = (
        alt.Chart(chart_data)
        .mark_bar()
        .encode(
            x=alt.X("indiv_mid_2023:Q", title="Animals in production (2023, mid estimate)"),
            y=alt.Y("Species_name:N", sort="-x", title=""),
            color=alt.Color(
                "Trajectory:N",
                scale=alt.Scale(
                    domain=["Gaining ground", "Losing ground"],
                    range=["#4caf50", "#e57373"],
                ),
                legend=alt.Legend(title=""),
            ),
            tooltip=[
                alt.Tooltip("Species_name:N",        title="Species"),
                alt.Tooltip("indiv_mid_2023:Q",       title="Animals farmed (2023)", format=","),
                alt.Tooltip("relative_CAGR_simple:Q", title="Rel. CAGR (%/yr)", format=".1f"),
            ],
        )
        .properties(
            title=f"{country}: {taxon} production by species (2023)  ·  Green = gaining  ·  Red = losing",
            height=max(200, len(chart_data) * 30),
        )
    )
    st.altair_chart(chart, use_container_width=True)


def page_country(detail, ens):
    st.caption("🌍  COUNTRY BREAKDOWN")
    st.title("Is this country's aquaculture portfolio consolidating or diversifying?")
    st.markdown(
        "Select a country to see portfolio direction, year-by-year species composition, "
        "and growth signals."
    )

    share_cols = get_share_cols(detail)
    years      = get_years(share_cols)

    all_countries    = sorted(detail["Country_name"].unique())
    selected_country = st.selectbox("Select country", all_countries)
    country_data     = detail[detail["Country_name"] == selected_country].copy()
    country_ens      = ens[ens["Country_name"] == selected_country]

    for taxon in ["Finfish", "Crustacean"]:
        taxon_data = country_data[country_data["taxon_group"] == taxon].copy()
        ens_rows   = country_ens[country_ens["taxon_group"] == taxon]
        ens_row    = ens_rows.iloc[0].to_dict() if not ens_rows.empty else None

        st.subheader(taxon)

        if ens_row:
            render_ens_badge(ens_row)
        st.write("")

        if taxon_data.empty:
            st.write(f"No {taxon.lower()} data for this country.")
            continue

        stable   = taxon_data[(taxon_data["status"] == "Stable") & (~taxon_data["is_volatile"])]
        n_stable = len(stable)
        if n_stable < 3:
            st.warning(
                f"**Low confidence in growth rankings · {taxon}:** "
                "rankings are based on a small number of comparable species."
            )
        else:
            st.info(
                f"**Moderate confidence · {taxon}:** based on {n_stable} species with "
                "consistent reporting. Relative growth vs. sector shows whether a species "
                "is outpacing or underperforming this country's sector.",
                icon="ℹ️",
            )

        render_share_heatmap(taxon_data, share_cols, years)
        render_bar_chart(taxon_data, selected_country, taxon)
        st.divider()


# ==============================================================================
# PAGE 3 — PORTFOLIO STABILITY (JACCARD)
# ==============================================================================

def page_jaccard(jaccard):
    st.caption("🔄  PORTFOLIO STABILITY")
    st.title("How much has each country's species portfolio changed?")
    st.markdown(
        "A score of **1.0** means the species mix in 2023 was identical to 2019: "
        "same species, same proportions. A score near **0** means the portfolio has "
        "almost completely turned over. Ranked from most changed to most stable."
    )

    top40 = jaccard[jaccard["manually_reviewed"] == True].copy()
    top40 = top40.sort_values("jaccard_2019_2023", ascending=True).reset_index(drop=True)

    display = pd.DataFrame({
        "Country":                           top40["country_name"].values,
        "Composition stability (2019-2023)": top40["jaccard_2019_2023"].values,
        "Species in 2019":                   top40["n_species_2019"].fillna(0).astype(int).values,
        "Species in 2023":                   top40["n_species_2023"].fillna(0).astype(int).values,
        "Individuals 2019":                  [format_individuals(v) for v in top40["total_indiv_2019"]],
        "Individuals 2023":                  [format_individuals(v) for v in top40["total_indiv_2023"]],
    })

    st.dataframe(
        display,
        use_container_width=True,
        hide_index=True,
        column_config={
            "Country": st.column_config.TextColumn("Country"),
            "Composition stability (2019-2023)": st.column_config.ProgressColumn(
                "Composition stability (2019-2023)",
                help=(
                    "Weighted Jaccard similarity between 2019 and 2023 species portfolios.\n\n"
                    "1.0 = identical composition. 0 = completely different.\n\n"
                    "Based on finfish and crustacean production only, using individual animal counts."
                ),
                min_value=0,
                max_value=1,
                format="%.2f",
            ),
            "Species in 2019": st.column_config.NumberColumn("Species in 2019"),
            "Species in 2023": st.column_config.NumberColumn("Species in 2023"),
            "Individuals 2019": st.column_config.TextColumn("Individuals 2019"),
            "Individuals 2023": st.column_config.TextColumn("Individuals 2023"),
        },
    )


# ==============================================================================
# NAVIGATION
# ==============================================================================

st.sidebar.markdown("## Pages")
st.sidebar.caption("Select a view below to explore the data.")

page = st.sidebar.radio(
    "Pages",
    ["Species Watchlist", "Country Breakdown", "Portfolio Stability"],
    label_visibility="collapsed",
)

st.sidebar.divider()

# ── Load data & route ──────────────────────────────────────────────────────────

detail, ens, jaccard = load_data()

if page == "Species Watchlist":
    page_watchlist(detail)
elif page == "Country Breakdown":
    page_country(detail, ens)
elif page == "Portfolio Stability":
    page_jaccard(jaccard)

# ==============================================================================
# SIDEBAR FOOTER — rendered last so it sits below all page-specific filters
# ==============================================================================

st.sidebar.divider()
st.sidebar.markdown("#### How to use this tool")
st.sidebar.markdown("""
**Start with the Species Watchlist** to identify species entering or exiting the portfolio.

**Use Country Breakdown** to see whether a country's portfolio is concentrating around a
dominant species or spreading across more, and which species are driving that change.

**Use Portfolio Stability** to compare how much each country's species mix has shifted
between 2019 and 2023.

Welfare weightings are intentionally excluded. This tool tracks structural change in the
industry: how the composition of aquaculture is shifting.
""")

st.sidebar.divider()
st.sidebar.caption(
    "**Data period:** 2019-2023. Updated annually when FAO releases new data.\n\n"
    "**Source:** FAO global aquaculture production statistics. Individual animal estimates "
    "derived from species-specific conversion factors.\n\n"
    "Note: FAO occasionally renames or recodes species between releases. If results look "
    "unexpected after a data update, check whether species codes have changed.\n\n"
    f"[📊 View raw data]({SHEET_URL})\n\n"
    "Questions? [cc8007@nyu.edu](mailto:cc8007@nyu.edu)"
)
