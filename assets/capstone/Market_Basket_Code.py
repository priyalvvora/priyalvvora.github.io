"""
Market Basket Analysis for Ruby and Roses (Apriori implementation)
==================================================================

Mines frequent itemsets and association rules from Shopify line-item data
to identify natural product bundles and cross-sell opportunities.

Output: a CSV of association rules with support, confidence, lift, and
conviction — sortable by any metric for downstream merchandising work.

Usage:
    python Market_Basket_Code.py --input baskets.csv --output rules.csv \\
        --min-support 0.002 --min-confidence 0.1 --min-lift 1.0

Input format (baskets.csv):
    One row per transaction, comma-separated list of product names.
    Example:
        Bundle Up,Charm + Mystery Skein,Apple Cider
        Cathedral Mini Set,A Christmas Surprise

Written for the Ruby and Roses capstone (Krannert, 2022).
"""

import argparse
import csv
from itertools import chain, combinations


# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------

def load_transactions(path):
    """Yield each transaction as a frozenset of items."""
    transactions = []
    with open(path, "r", newline="", encoding="utf-8") as f:
        for line in f:
            items = [x.strip() for x in line.strip().rstrip(",").split(",") if x.strip()]
            if items:
                transactions.append(frozenset(items))
    return transactions


def initial_itemset(transactions):
    """Build the candidate 1-itemset (every distinct product as a singleton)."""
    itemset = set()
    for txn in transactions:
        for item in txn:
            itemset.add(frozenset([item]))
    return itemset


# ---------------------------------------------------------------------------
# Apriori core
# ---------------------------------------------------------------------------

def filter_by_support(transactions, candidates, min_support):
    """Return {itemset: support} for candidates that meet the threshold."""
    n = len(transactions)
    out = {}
    for item in candidates:
        support = sum(1 for txn in transactions if item.issubset(txn)) / n
        if support >= min_support:
            out[item] = support
    return out


def join_step(itemset, k):
    """Generate candidate k-itemsets from frequent (k-1)-itemsets."""
    return {a.union(b) for a in itemset for b in itemset if len(a.union(b)) == k}


def frequent_itemsets(transactions, min_support):
    """Iteratively grow frequent k-itemsets until none meet support."""
    candidates = initial_itemset(transactions)
    frequent = filter_by_support(transactions, candidates, min_support)
    k = 2
    while frequent:
        new_candidates = join_step(set(frequent.keys()), k)
        new_frequent = filter_by_support(transactions, new_candidates, min_support)
        if not new_frequent:
            break
        frequent.update(new_frequent)
        k += 1
    return frequent


# ---------------------------------------------------------------------------
# Rule generation
# ---------------------------------------------------------------------------

def all_subsets(itemset):
    """Every non-empty proper subset of an itemset."""
    items = list(itemset)
    return chain(*[combinations(items, r + 1) for r in range(len(items))])


def generate_rules(frequent, min_confidence, min_lift):
    """Build association rules from frequent itemsets meeting thresholds."""
    rules = []
    for itemset, support in frequent.items():
        if len(itemset) < 2:
            continue
        for antecedent in all_subsets(itemset):
            antecedent = frozenset(antecedent)
            consequent = itemset - antecedent
            if not consequent:
                continue
            confidence = support / frequent[antecedent]
            lift = confidence / frequent[consequent]
            if confidence < min_confidence or lift < min_lift:
                continue
            conviction = (
                float("inf") if confidence == 1
                else (1 - frequent[consequent]) / (1 - confidence)
            )
            rules.append({
                "antecedent": ", ".join(sorted(antecedent)),
                "consequent": ", ".join(sorted(consequent)),
                "support": round(support, 6),
                "confidence": round(confidence, 6),
                "lift": round(lift, 4),
                "conviction": "Infinity" if conviction == float("inf") else round(conviction, 4),
            })
    return rules


def write_rules(rules, path):
    """Write rules CSV sorted by lift (descending)."""
    if not rules:
        print("No rules met the thresholds.")
        return
    rules = sorted(rules, key=lambda r: r["lift"] if r["lift"] != float("inf") else 1e12, reverse=True)
    fieldnames = ["antecedent", "consequent", "support", "confidence", "lift", "conviction"]
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames)
        w.writeheader()
        w.writerows(rules)
    print(f"  Wrote {len(rules):,} rules to {path}")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def parse_args():
    p = argparse.ArgumentParser(description="Market basket analysis (Apriori)")
    p.add_argument("--input", required=True, help="CSV file: one transaction per line, comma-separated items")
    p.add_argument("--output", required=True, help="Where to write the rules CSV")
    p.add_argument("--min-support", type=float, default=0.002, help="Minimum support (default 0.002)")
    p.add_argument("--min-confidence", type=float, default=0.1, help="Minimum confidence (default 0.1)")
    p.add_argument("--min-lift", type=float, default=1.0, help="Minimum lift (default 1.0)")
    return p.parse_args()


def main():
    args = parse_args()
    print(f"Loading transactions from {args.input} ...")
    transactions = load_transactions(args.input)
    print(f"  {len(transactions):,} transactions")

    print(f"Mining frequent itemsets (min_support={args.min_support}) ...")
    frequent = frequent_itemsets(transactions, args.min_support)
    print(f"  {len(frequent):,} frequent itemsets")

    print(f"Generating rules (min_confidence={args.min_confidence}, min_lift={args.min_lift}) ...")
    rules = generate_rules(frequent, args.min_confidence, args.min_lift)
    write_rules(rules, args.output)


if __name__ == "__main__":
    main()
