"""
RFM Segmentation for Ruby and Roses (Shopify customer data)
============================================================

Calculates Recency, Frequency, Monetary Value quartiles for each customer
and assigns a named behavioral segment used for downstream marketing playbooks.

Output: a CSV with one row per customer containing R/F/M quartiles, the
combined RFMClass, and a human-readable segment name.

Usage:
    python RFM_Analysis_Code.py --input orders.csv --output rfm_output.csv --asof 2022-02-28

Quartile scoring approach inspired by Joao Correia's RFM-analysis (Apache 2.0).
Segment-naming logic written for the Ruby and Roses consulting engagement.
"""

import argparse
from datetime import datetime

import pandas as pd


# ---------------------------------------------------------------------------
# Quartile scoring
# ---------------------------------------------------------------------------

def r_class(value, quartiles, col):
    """Recency: lower is better, so quartile 1 = most recent."""
    if value <= quartiles[col][0.25]:
        return 1
    elif value <= quartiles[col][0.50]:
        return 2
    elif value <= quartiles[col][0.75]:
        return 3
    return 4


def fm_class(value, quartiles, col):
    """Frequency & Monetary: higher is better, so quartile 1 = most valuable."""
    if value <= quartiles[col][0.25]:
        return 4
    elif value <= quartiles[col][0.50]:
        return 3
    elif value <= quartiles[col][0.75]:
        return 2
    return 1


# ---------------------------------------------------------------------------
# Segment naming
# ---------------------------------------------------------------------------

def segment_name(rfm_class: str) -> str:
    """Map an RFMClass triple (e.g. '111') to a marketing-friendly segment.

    Logic (first match wins):
      - 111 / 112 / 121 / 122  -> Top Customers
      - 211 / 212 / 221        -> Good Customers
      - 411 / 412              -> Top Customers that haven't made a purchase recently
      - F == 1                 -> Frequently purchase customers
      - M == 1                 -> High Monetary Value Customers
      - 444                    -> Poor Lifetime Customer Value
      - R == 1                 -> Recent Customers
      - everything else        -> Average Customers
    """
    r, f, m = int(rfm_class[0]), int(rfm_class[1]), int(rfm_class[2])

    if rfm_class in {"111", "112", "121", "122"}:
        return "Top Customers"
    if rfm_class in {"211", "212", "221"}:
        return "Good Customers"
    if rfm_class in {"411", "412"}:
        return "Top Customers that haven't made a purchase recently"
    if f == 1:
        return "Frequently purchase customers"
    if m == 1:
        return "High Monetary Value Customers"
    if rfm_class == "444":
        return "Poor Lifetime Customer Value"
    if r == 1:
        return "Recent Customers"
    return "Average Customers"


# ---------------------------------------------------------------------------
# Main pipeline
# ---------------------------------------------------------------------------

def build_rfm(orders: pd.DataFrame, as_of: datetime) -> pd.DataFrame:
    """Aggregate orders into an RFM table and assign segments."""
    orders = orders.copy()
    orders["order_date"] = pd.to_datetime(orders["order_date"])

    rfm = orders.groupby("customer").agg(
        recency=("order_date", lambda x: (as_of - x.max()).days),
        frequency=("order_id", "count"),
        monetary_value=("grand_total", "sum"),
    )

    quartiles = rfm.quantile(q=[0.25, 0.5, 0.75]).to_dict()

    rfm["R_Quartile"] = rfm["recency"].apply(lambda v: r_class(v, quartiles, "recency"))
    rfm["F_Quartile"] = rfm["frequency"].apply(lambda v: fm_class(v, quartiles, "frequency"))
    rfm["M_Quartile"] = rfm["monetary_value"].apply(lambda v: fm_class(v, quartiles, "monetary_value"))

    rfm["RFMClass"] = (
        rfm["R_Quartile"].astype(str)
        + rfm["F_Quartile"].astype(str)
        + rfm["M_Quartile"].astype(str)
    )
    rfm["Customer Segment"] = rfm["RFMClass"].apply(segment_name)

    return rfm.reset_index()


def summarize_segments(rfm: pd.DataFrame) -> pd.DataFrame:
    """Roll up segment counts and value contribution for reporting."""
    return (
        rfm.groupby("Customer Segment")
        .agg(
            customers=("customer", "count"),
            avg_recency=("recency", "mean"),
            avg_frequency=("frequency", "mean"),
            total_monetary=("monetary_value", "sum"),
            avg_monetary=("monetary_value", "mean"),
        )
        .sort_values("customers", ascending=False)
        .round(2)
    )


def parse_args():
    p = argparse.ArgumentParser(description="RFM segmentation for Shopify orders")
    p.add_argument("--input", required=True, help="Path to orders CSV")
    p.add_argument("--output", required=True, help="Path to write RFM segmentation CSV")
    p.add_argument("--asof", required=True, help="Reference date YYYY-MM-DD for recency calc")
    return p.parse_args()


def main():
    args = parse_args()
    as_of = datetime.strptime(args.asof, "%Y-%m-%d")

    print(f"Loading orders from {args.input} ...")
    orders = pd.read_csv(args.input)
    print(f"  {len(orders):,} rows")

    print(f"Calculating RFM as of {args.asof} ...")
    rfm = build_rfm(orders, as_of)
    rfm.to_csv(args.output, index=False)
    print(f"  Wrote {len(rfm):,} customer rows to {args.output}")

    print("\nSegment summary:")
    print(summarize_segments(rfm).to_string())


if __name__ == "__main__":
    main()
