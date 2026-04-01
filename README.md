# Aquaculture Portfolio Tracker

A Streamlit dashboard tracking structural change in global aquaculture for fishes and crustaceans. It answers three questions:

- Which species are gaining or losing ground across the industry?
- Is a given country's portfolio diversifying or consolidating?
- How much has each country's species portfolio composition shifted since 2019?

Live app: (https://aquaculture-portfolio-tracker.streamlit.app/)
Data source: [View raw data](https://docs.google.com/spreadsheets/d/10HE5wznbTTdQc5eDc-JPFzkLi7dQJXnx/)

---

## Repository structure

```
app_portfolio.py          Streamlit app (three pages)
streamlit_datasheet.R     Master data pipeline that produces the Excel for Google Sheets
requirements.txt          Python dependencies

data/
  FAO_allyears_soc.csv                              Raw FAO production data with welfare flags and tonnage-individual conversion numbers
  nei/
    NEI_allocation_complete.R   NEI-allocation algoritm dataset (three scenarios)
    



## Contact

cc8007@nyu.edu
