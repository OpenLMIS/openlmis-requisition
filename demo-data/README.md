# Demo Data for OpenLMIS Requisition Service
This folder holds demo data for the requisition service. The demo data is used by developers, QA
staff, and is automatically loaded into some environments for demo and testing purposes. It is not
for use in production environments.

Each .json file contains demo data that corresponds to one database table.

## Requisition Templates

Defined in requisition.requisition_templates.json and requisition.columns_maps.json.

1. Family Planning
  * Product Code
  * Product Name
  * Beginning Balance
  * Total Received Quantity
  * Total Consumed Quantity
  * Total Losses And Adjustments (with stock adjustment reasons from Reference Data demo data)
  * Total Stockout Days
  * Stock On Hand (calculated)
  * Average Consumption (periods to average from: 3)
  * Calculated Order Quantity
  * Price Per Pack
  * Dispensing Unit
  * Requested Quantity
  * Requested Quantity Explanation
  * Approved Quantity
  * Remarks
2. Essential Meds (currently the template is nearly identical to Family Planning, with Unit/Unit of
  Issue the only difference)
  * Product Code
  * Product Name
  * Beginning Balance
  * Total Received Quantity
  * Total Losses And Adjustments (with stock adjustment reasons from Reference Data demo data)
  * Total Stockout Days
  * Stock On Hand (user input)
  * Total Consumed Quantity (calculated)
  * Average Consumption (periods to average from: 3)
  * Calculated Order Quantity
  * Price Per Pack
  * Unit of Issue
  * Packs To Ship
  * Requested Quantity
  * Requested Quantity Explanation
  * Approved Quantity
  * Remarks
3. New Program (identical to Essential Meds template)
  * Product Code
  * Product Name
  * Beginning Balance
  * Total Received Quantity
  * Total Consumed Quantity
  * Total Losses And Adjustments (with stock adjustment reasons from Reference Data demo data)
  * Stock On Hand (calculated)
  * Requested Quantity
  * Requested Quantity Explanation
  * Approved Quantity
  * Remarks
  * Total Stockout Days
  * Total
  * Packs To Ship
  * Price Per Pack
  * Number of New Patients Added
  * Total Cost
  * Adjusted Consumption
  * Average Consumption
  * Maximum Stock Quantity
  * Calculated Order Quantity

## Requisitions

This demo data, requisition.requisitions.json and .requisition_line_items.json, contains a few
example requisitions in different statuses. To truly demo the software well, it may be helpful
to log in and approve these to clear them out before beginning your demo.

1. Family Planning
  * 9 requisitions in many statuses: Initiated, Released, Authorized, Approved, and Skipped
2. Essential Meds 
  * 4 requisitions in these statuses: Released, Authorized, and Approved

Currently, all those requisitions are Normal, not Emergency requisitions.

Facilities, Programs, Products, Requisition Groups and User Roles & Rights come from the
[Reference Data service's demo data](https://github.com/OpenLMIS/openlmis-referencedata/tree/master/demo-data).
