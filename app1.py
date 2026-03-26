import re
import streamlit as st
import pandas as pd

st.set_page_config(layout="wide", page_title="Aquaculture Species Tracker | Animal Welfare")

SPECIES_COUNTRY_DETAIL_URL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQQk_eKJ1YezQqLiEObOcvtdNsXpRNgu954LIZyQq7yxGtnf486vCQN3q-O-oavCA/pub?gid=1512611148&single=true&output=csv"
FLAGGED_SPECIES_URL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQQk_eKJ1YezQqLiEObOcvtdNsXpRNgu954LIZyQq7yxGtnf486vCQN3q-O-oavCA/pub?gid=346524902&single=true&output=csv"


@st.cache_data(ttl=3600)
def load_species_country_detail():
    return pd.read_csv(SPECIES_COUNTRY_DETAIL_URL)



def get_year_range(detail):
    years = [int(m.group(1)) for c in detail.columns if (m := re.match(r"indiv_mid_(\d{4})$", c))]
    return min(years), max(years)


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


def compute_species_summary(detail):
    stable = detail[(detail["status"] == "Stable") & (~detail["is_volatile"])].copy()

    if stable.empty:
        return pd.DataFrame()

    def agg_species(g):
        total_mid = g["indiv_mid_2023"].sum()
        valid_rcagr = g.dropna(subset=["relative_CAGR_simple"])
        valid_sc = g.dropna(subset=["share_change_pp"])

        rcagr_denom = valid_rcagr["indiv_mid_2023"].sum()
        sc_denom = valid_sc["indiv_mid_2023"].sum()

        return pd.Series({
            "total_indiv_mid_end": total_mid,
            "total_indiv_lower_end": g["indiv_lower_2023"].sum(),
            "total_indiv_upper_end": g["indiv_upper_2023"].sum(),
            "n_countries_present": (g["indiv_mid_2023"] > 0).sum(),
            "n_countries_outpacing": (valid_rcagr["relative_CAGR_simple"] > 0).sum(),
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


def build_display_df(summary):
    return pd.DataFrame({
        "Species": summary["Species_name"].values,
        "Scientific name": summary["Scientific_Name"].values,
        "Group": summary["taxon_group"].values,
        "Animals farmed (2023, mid estimate)": [format_individuals(v) for v in summary["total_indiv_mid_end"]],
        "Relative growth vs. sector (%/yr)": summary["weighted_relative_CAGR_simple"].round(1).values,
        "Sector share change (percentage points)": summary["weighted_share_change_pp"].round(1).values,
        "Countries outpacing sector": summary["n_countries_outpacing"].astype(int).values,
        "Countries losing ground": summary["n_countries_losing_ground"].astype(int).values,
    })


def page_watchlist():
    st.title("Which aquaculture species are expanding, and which are contracting?")

    st.markdown("""
Each species gaining ground in aquaculture is a potential **new target population** that will 
likely require independent welfare research and intervention infrastructure before any animal in it 
can be meaningfully helped. **Species losing ground are the opposite signal**: a potentially shrinking window 
for impact, or a reason to reconsider sustained investment.

This table ranks species by how fast they are gaining or losing share *relative to the sector as a whole*,
not just whether they are growing in absolute terms. A species growing at 5%/yr in a sector growing 
at 10%/yr is losing strategic ground even if its absolute numbers are rising.
""")

    st.info(
        "**Only species with consistent reporting across the full data period are ranked.** "
        "Species with erratic year-to-year data are included in total animal estimates but excluded "
        "from growth rankings, since their trend cannot be reliably determined.",
        icon="ℹ️"
    )

    detail = load_species_country_detail()

    # --- Sidebar filters ---
    all_countries = sorted(detail["Country_name"].unique())
    with st.sidebar.expander(f"Select countries ({len(all_countries)} of {len(all_countries)} selected)", expanded=False):
        selected_countries = st.multiselect(
            "Countries",
            options=all_countries,
            default=all_countries,
            label_visibility="collapsed",
        )

    taxon_filter = st.sidebar.radio("Taxon group", ["All", "Finfish", "Crustacean"])
    min_countries = st.sidebar.slider("Minimum countries with data", 1, 20, 1)
    multi_only = st.sidebar.checkbox("Multi-country signal only", value=False,
                                     help="Show only species outpacing the sector in 2+ countries. Multi-country signals are more reliable than single-country ones.")
    min_scale_m = st.sidebar.slider("Minimum scale (millions of individuals)", 0, 1000, 1000)

    # --- Filter and compute ---
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

    gaining = summary[summary["weighted_relative_CAGR_simple"] > 0].sort_values("weighted_share_change_pp", ascending=False)
    losing = summary[summary["weighted_relative_CAGR_simple"] <= 0].sort_values("weighted_share_change_pp", ascending=False)

    # --- Gaining ground ---
    st.markdown("### 📈 Gaining ground on the sector")
    st.caption(
        "These species are growing faster than aquaculture overall. A species climbing this list across "
        "multiple countries may be entering the portfolio problem, as it may be a new target population the movement "
        "will eventually need to cover independently."
    )

    _watchlist_col_config = {
        "Species": st.column_config.TextColumn("Species", help="Common name of the species."),
        "Scientific name": st.column_config.TextColumn("Scientific name", help="Latin (scientific) name of the species."),
        "Group": st.column_config.TextColumn("Group", help="Broad taxonomic group: Finfish or Crustacean."),
        "Animals farmed (2023, mid estimate)": st.column_config.TextColumn(
            "Animals farmed (2023, mid estimate)",
            help="Mid-point estimate of the number of individuals farmed in 2023."
        ),
        "Relative growth vs. sector (%/yr)": st.column_config.NumberColumn(
            "Relative growth vs. sector (%/yr)",
            help="How fast this species is growing relative to aquaculture as a whole. "
                 "Positive = gaining share of the sector; negative = losing ground. "
                 "A species growing at 5%/yr in a 10%/yr sector is still losing strategic ground."
        ),
        "Sector share change (percentage points)": st.column_config.NumberColumn(
            "Sector share change (percentage points)",
            help="Change in this species' share of total aquaculture production over the data period, in percentage points."
        ),
        "Countries outpacing sector": st.column_config.NumberColumn(
            "Countries outpacing sector",
            help="Number of countries where this species is growing faster than that country's aquaculture sector overall. "
                 "A signal appearing in multiple countries is more reliable than a single-country signal."
        ),
        "Countries losing ground": st.column_config.NumberColumn(
            "Countries losing ground",
            help="Number of countries where this species is growing more slowly than that country's aquaculture sector overall."
        ),
    }

    if gaining.empty:
        st.write("No species gaining ground under current filters.")
    else:
        st.dataframe(build_display_df(gaining), use_container_width=True, hide_index=True,
                     column_config=_watchlist_col_config)

        st.markdown("##### Country-level breakdown — gaining species")
        for _, row in gaining.iterrows():
            species_data = filtered[filtered["Species_name"] == row["Species_name"]].copy()
            outpacing = species_data[
                species_data["relative_CAGR_simple"].notna() &
                (species_data["relative_CAGR_simple"] > 0)
            ][["Country_name", "relative_CAGR_simple", "indiv_mid_2023"]].sort_values(
                "relative_CAGR_simple", ascending=False
            )
            n = len(outpacing)
            label = f"{row['Species_name']} — {n} {'country' if n == 1 else 'countries'} outpacing"
            with st.expander(label):
                if outpacing.empty:
                    st.write("No country-level data available.")
                else:
                    outpacing.columns = ["Country", "Relative growth vs. sector (%/yr)", "Animals farmed (2023, mid estimate)"]
                    outpacing["Animals farmed (2023, mid estimate)"] = outpacing["Animals farmed (2023, mid estimate)"].apply(format_individuals)
                    st.dataframe(outpacing, use_container_width=True, hide_index=True)

    st.divider()

    # --- Losing ground ---
    st.markdown("### 📉 Losing ground")
    st.caption(
        "These species are growing more slowly than the sector, or shrinking. This may indicate a narrowing "
        "window for impact — or a signal to reconsider sustained investment in a species whose share of the "
        "industry is declining."
    )

    if losing.empty:
        st.write("No species losing ground under current filters.")
    else:
        st.dataframe(build_display_df(losing), use_container_width=True, hide_index=True,
                     column_config=_watchlist_col_config)


def page_country():
    st.title("Country breakdown")

    st.markdown("""
Select a country to see every farmed species reported, how fast each is growing, and whether it is 
expanding faster or slower than that country's sector as a whole.

**How to use this alongside the Species Watchlist:** if a species is gaining ground globally, this 
page tells you where that momentum is concentrated — which is where engagement is most timely. If a 
species is losing ground globally, this page can show whether that retreat is happening everywhere 
or whether a specific country is bucking the trend.
""")

    detail = load_species_country_detail()
    all_countries = sorted(detail["Country_name"].unique())

    selected_country = st.selectbox("Select country", all_countries)

    country_data = detail[detail["Country_name"] == selected_country].copy()

    for taxon in ["Finfish", "Crustacean"]:
        taxon_data = country_data[country_data["taxon_group"] == taxon].copy()
        if taxon_data.empty:
            continue

        st.subheader(taxon)

        stable = taxon_data[(taxon_data["status"] == "Stable") & (~taxon_data["is_volatile"])]
        n_stable = len(stable)
        volatile = taxon_data[taxon_data["is_volatile"]]

        if n_stable < 3:
            st.warning(
                f"Low confidence in growth rankings · {taxon} — "
                f"rankings are based on a small number of comparable species. "
                "Treat directional signals as indicative rather than definitive."
            )
        else:
            sector_cagr = stable["relative_CAGR_simple"].mean()
            st.info(
                f"**Moderate confidence in growth rankings · {taxon}** — "
                f"based on {n_stable} species with consistent reporting. "
                "**Relative growth vs. sector** shows whether a species is outpacing or underperforming "
                "this country's overall aquaculture sector. Positive = gaining share; negative = losing ground.",
                icon="ℹ️"
            )

        display_cols = ["Species_name", "status", "indiv_mid_2023", "relative_CAGR_simple", "share_change_pp"]
        available = [c for c in display_cols if c in taxon_data.columns]
        display = taxon_data[available].copy()
        display.columns = (
            ["Species", "Status", "Animals farmed (2023, mid estimate)",
             "Relative growth vs. sector (%/yr)", "Sector share change (percentage points)"][:len(available)]
        )
        if "Status" in display.columns and "is_volatile" in taxon_data.columns:
            display["Status"] = [
                "Volatile — excluded from rankings" if v else s
                for v, s in zip(taxon_data["is_volatile"].values, taxon_data["status"].values)
            ]
        if "Animals farmed (2023, mid estimate)" in display.columns:
            display["Animals farmed (2023, mid estimate)"] = display["Animals farmed (2023, mid estimate)"].apply(format_individuals)

        display = display.sort_values("Relative growth vs. sector (%/yr)", ascending=False, na_position="last")
        st.dataframe(display, use_container_width=True, hide_index=True, column_config={
            "Species": st.column_config.TextColumn("Species", help="Common name of the species."),
            "Status": st.column_config.TextColumn(
                "Status",
                help="Whether this species has consistent enough data to rank. "
                     "'Stable' = included in growth rankings. Volatile or sparse series are shown here "
                     "but excluded from rankings since their trend cannot be reliably determined."
            ),
            "Animals farmed (2023, mid estimate)": st.column_config.TextColumn(
                "Animals farmed (2023, mid estimate)",
                help="Mid-point estimate of the number of individuals farmed in 2023."
            ),
            "Relative growth vs. sector (%/yr)": st.column_config.NumberColumn(
                "Relative growth vs. sector (%/yr)",
                help="How fast this species is growing relative to this country's aquaculture sector as a whole. "
                     "Positive = gaining share; negative = losing ground. "
                     "A species growing at 5%/yr in a 10%/yr sector is still losing strategic ground."
            ),
            "Sector share change (percentage points)": st.column_config.NumberColumn(
                "Sector share change (percentage points)",
                help="Change in this species' share of the country's total aquaculture production "
                     "over the data period, in percentage points."
            ),
        })

        # Bar chart: gaining = green, losing = red
        chart_data = taxon_data[["Species_name", "indiv_mid_2023", "relative_CAGR_simple"]].dropna(
            subset=["indiv_mid_2023"]
        ).copy()
        chart_data["color"] = chart_data["relative_CAGR_simple"].apply(
            lambda x: "Expanding faster than sector" if (pd.notna(x) and x > 0) else "Losing ground"
        )
        if not chart_data.empty:
            import altair as alt
            chart = (
                alt.Chart(chart_data)
                .mark_bar()
                .encode(
                    x=alt.X("indiv_mid_2023:Q", title="Animals in production (2023, mid estimate)"),
                    y=alt.Y("Species_name:N", sort="-x", title=""),
                    color=alt.Color(
                        "color:N",
                        scale=alt.Scale(
                            domain=["Expanding faster than sector", "Losing ground"],
                            range=["#4caf50", "#e57373"]
                        ),
                        legend=alt.Legend(title=""),
                    ),
                    tooltip=["Species_name", "indiv_mid_2023", "relative_CAGR_simple"]
                )
                .properties(
                    title=f"{selected_country}: {taxon} production by species (2023) · Green = expanding faster than sector · Red = losing ground",
                    height=max(200, len(chart_data) * 30)
                )
            )
            st.altair_chart(chart, use_container_width=True)


# --- Sidebar navigation and context ---
st.sidebar.markdown("## Navigate")
page = st.sidebar.radio(
    "Navigate",
    ["Species Watchlist", "Country Breakdown"],
    label_visibility="collapsed",
)

st.sidebar.divider()

st.sidebar.markdown("#### How to use this tool")
st.sidebar.markdown("""
**Start with the Species Watchlist** to identify species entering or exiting the portfolio. Gaining ground signals a new target population forming; losing ground signals a shrinking window.

**Use Country Breakdown** to find where a species' momentum is concentrated, and where 
engagement is most timely.

Welfare weightings are intentionally excluded. This tool tracks **structural change in the 
industry** (ie how the composition of aquaculture is shifting), not welfare severity within 
any given species.
""")

st.sidebar.divider()

if page in ["Species Watchlist", "Country Breakdown"]:
    detail = load_species_country_detail()
    start_year, end_year = get_year_range(detail)
    st.sidebar.caption(
        f"**Data period:** {start_year}–{end_year}. Updated annually when FAO releases new data.\n\n"
        "**Source:** FAO global aquaculture production statistics. Individual animal estimates derived "
        "from species-specific conversion factors.\n\n"
        "Note: FAO occasionally renames or recodes species between releases — if results look "
        "unexpected after a data update, check whether species codes have changed."
    )

# --- Route pages ---
if page == "Species Watchlist":
    page_watchlist()
elif page == "Country Breakdown":
    page_country()