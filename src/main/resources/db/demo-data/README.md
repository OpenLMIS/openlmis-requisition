# Demo Data for OpenLMIS Requisition Service

See the project README for how to load this data.

The directory `schemas/` holds copies of the Mockaroo schemas used for generating
data. When the set of schemas on Mockaroo changes, please revision them here.

This folder holds demo data for the requisition service. The demo data is used by developers, QA
staff, and is automatically loaded into some environments for demo and testing purposes. It is not
for use in production environments.

Each .csv file contains demo data that corresponds to one database table.

## Requisition Templates

Defined in requisition.requisition_templates.csv and requisition.columns_maps.csv.

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
4. EPI
  * Product Code
  * Product Name
  * Beginning Balance
  * Total Received Quantity
  * Total Consumed Quantity (calculated)
  * Total Losses And Adjustments (with stock adjustment reasons from Reference Data demo data)
  * Total Stockout Days
  * Stock On Hand
  * Average Consumption (periods to average from: 3)
  * Calculated Order Quantity
  * Price Per Pack
  * Dispensing Unit
  * Requested Quantity
  * Requested Quantity Explanation
  * Approved Quantity
  * Remarks
  * Total
  * Total Cost
  * Adjusted Consumption

## Requisitions

This demo data, requisition.requisitions.csv and requisition.requisition_line_items.csv, contains a few
example requisitions in different statuses. To truly demo the software well, it may be helpful
to log in and approve these to clear them out before beginning your demo.

|Program        |Facility                |Period |Status    |Emergency|
|---------------|------------------------|-------|----------|---------|
|Essential Meds |Balaka District Hospital|2017Q1 |APPROVED  | false   |
|Essential Meds |Balaka District Hospital|2017Q2 |RELEASED  | false   |
|Essential Meds |Balaka District Hospital|2017Q3 |RELEASED  | false   |
|Essential Meds |Kankao Health Facility  |2017Q1 |APPROVED  | false   |
|Family Planning|Balaka District Hospital|2017Q1 |APPROVED  | false   |
|Family Planning|Balaka District Hospital|2017Q2 |AUTHORIZED| false   |
|Family Planning|Comfort Health Clinic   |Jan2017|AUTHORIZED| false   |
|Family Planning|Comfort Health Clinic   |Feb2017|AUTHORIZED| false   |
|Family Planning|Comfort Health Clinic   |Mar2017|AUTHORIZED| false   |
|Family Planning|Comfort Health Clinic   |Apr2017|INITIATED | false   |
|Family Planning|Kankao Health Facility  |2017Q1 |RELEASED  | true    |
|Family Planning|Kankao Health Facility  |2017Q1 |RELEASED  | false   |
|Family Planning|Kankao Health Facility  |2017Q2 |SKIPPED   | false   |
|Family Planning|Kankao Health Facility  |2017Q3 |APPROVED  | false   |
|EPI            |Cuamba, Cuamba          |Jan2017|APPROVED  | false   |
|EPI            |Lurio, Cuamba           |Jan2017|APPROVED  | false   |

Facilities, Programs, Products, Requisition Groups and User Roles & Rights come from the
[Reference Data service's demo data](https://github.com/OpenLMIS/openlmis-referencedata/tree/master/src/main/resources/db/demo-data).
