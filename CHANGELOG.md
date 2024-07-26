8.3.8 / 2024-04-19
==================
Bug fixes:
* [OLMIS-7860](https://openlmis.atlassian.net/browse/OLMIS-7860): Fix error with updating Requisition Template
* [OLMIS-7793](https://openlmis.atlassian.net/browse/OLMIS-7793): Fix bug with invalid AMC calculation for stock management based facilities
  
Improvements:
* [OLMIS-7858](https://openlmis.atlassian.net/browse/OLMIS-7793): Add four indexes on requisition schema

New functionality:
* [SELV3-705](https://openlmis.atlassian.net/browse/SELV3-705): Add endpoint to retrieve number of requisitions for approval
* [SELV3-718](https://openlmis.atlassian.net/browse/SELV3-718): Add filtering by facility and period for requisitions for approval
* [OAM-17](https://openlmis.atlassian.net/browse/OAM-17): Add endpoint to count requisitions with their statuses for all periods per facility

8.3.7 / 2023-11-07
==================
New functionality added in a backwards-compatible manner:
* [TZUP-432](https://openlmis.atlassian.net/browse/TZUP-432): Added new columns based on the Patient(s) tab to the Requisition Template.

Bug fixes:
* [TZUP-363](https://openlmis.atlassian.net/browse/TZUP-363): Fix bug related to 'Report only flag when processing period is checked and requisition report only is unchecked'
* [OLMIS-7808](https://openlmis.atlassian.net/browse/OLMIS-7808): Fix bug related to 'Issues with second approval of requisition'

8.3.6 / 2023-06-26
==================

Bug fixes:
* [OLMIS-7723](https://openlmis.atlassian.net/browse/OLMIS-7723): Fix error related to 'Approve buttons not visible when trying to approve requisition'
* [OLMIS-7772](https://openlmis.atlassian.net/browse/OLMIS-7772): Fix issues with new Chrome popover functionality

8.3.5 / 2023-04-05
==================

New functionality added in a backwards-compatible manner:
* [TZUP-274](https://openlmis.atlassian.net/browse/TZUP-274): Added backend code for requisition report only flag in requisition template.

Bug fixes:
* [OLMIS-7625](https://openlmis.atlassian.net/browse/OLMIS-7625): Fixed setting Start Date for a specific program in a specific facility.

8.3.4 / 2022-04-21
==================

Breaking changes:
* [OLMIS-7472](https://openlmis.atlassian.net/browse/OLMIS-7472): Upgrade postgres to v12

Improvements:
* [OLMIS-7521](https://openlmis.atlassian.net/browse/OLMIS-7521): Publish requisition service to central repository
* [OLMIS-7568](https://openlmis.atlassian.net/browse/OLMIS-7568): Use openlmis/dev:8 and openlmis/service-base:6.1

Bug fixes:
* [OLMIS-7499](https://openlmis.atlassian.net/browse/OLMIS-7499): Requisitions cannot be approved if the requisition template has no 'Remarks' column set.

8.3.3 / 2021-10-29
==================

Improvements:
* [OLMIS-7273](https://openlmis.atlassian.net/browse/OLMIS-7273): Improve performance of Requisition initialization
* [TZUP-205](https://openlmis.atlassian.net/browse/TZUP-205): send unskipped requisition line items to initiator

Bug fixes:
* [OLMIS-7376](https://openlmis.atlassian.net/browse/OLMIS-7376): Requisitions in approval queue shows only for user with REQUISITION_APPROVE role.

8.3.2 / 2021-05-27
==================

Improvements:
* [TZUP-123](https://openlmis.atlassian.net/browse/TZUP-123): persisting & email notification 
  of unskipped requisition line items
* [TZUP-122](https://openlmis.atlassian.net/browse/TZUP-122): changes to enable unskipping skipped requisition line tiems during approval
* [OLMIS-7219](https://openlmis.atlassian.net/browse/OLMIS-7219): Set Hibernate batch insert properties

8.3.1 / 2021-02-22
==================

TZ Upgrade
* [TZUP-156](https://openlmis.atlassian.net/browse/TZUP-156): Backend to store reason for R&R been rejected.


8.3.1 / 2020-11-16
==================

Bug fixes:
* [OLMIS-6868](https://openlmis.atlassian.net/browse/OLMIS-6868): Allowed approval of requisition to locally fulfilled facilities without additional fulfillment role.
* [OLMIS-6866](https://openlmis.atlassian.net/browse/OLMIS-6866): Fixed Beginning balance empty column issue for Stock Based Requisitions.
* [OLMIS-6876](https://openlmis.atlassian.net/browse/OLMIS-6876): Fixed order created without releasing requisition

8.3.0 / 2020-06-01
==================
New functionality added in a backwards-compatible manner:
* [OLMIS-6784](https://openlmis.atlassian.net/browse/OLMIS-6784): Update Spring Boot version to 2.x:
  * Spring Boot version is 2.2.2.
  * Flyway is at 6.0.8, new mechanism for loading Spring Security for OAuth2 (matching Spring Boot version), new versions for REST Assured, RAML tester, RAML parser, PowerMock, Mockito (so tests will pass) and Java callback mechanism has changed to a general handle() method.
  * Add lombok plugin in order to get build to recognize lombok annotations. 
  * Spring application properties for Flyway have changed.
  * Re-implement generation of Jasper reports.
  * Fix repository method signatures (findOne is now findById, etc.); additionally they return Optional.
  * Fix unit tests.
  * Fix integration tests. Use TestApplication which sets default redis server in order to get the tests to run.
  * API definitions require "Keep-Alive" header for web integration tests.

8.2.2 / 2020-04-14
==================

Improvements:
* [OLMIS-6741](https://openlmis.atlassian.net/browse/OLMIS-6741): Improved performance of finding processing periods on requisition initiate:
  * removed searching for processing schedules to verify processing period on requisition initiate as referencedata service returns periods with proper schedules.
* [OLMIS-6793](https://openlmis.atlassian.net/browse/OLMIS-6793): Allowed Calculated Order Quantity ISA column to be used in non Stock Based Requisitions.

Bug fixes:
* [OLMIS-6776](https://openlmis.atlassian.net/browse/OLMIS-6776): Fixed the issue with invalid token error:
  * A mechanism to retry authentication load after encountering the error was introduced.
* [OLMIS-6812](https://openlmis.atlassian.net/browse/OLMIS-6812): Fixed issue with endpoint '/api/requisitions/search' with 'modifiedDateFrom' parameter not returning all data:
  * Updated requisition to set the value of created date as the modified date when initiating.

8.2.1 / 2019-12-19
==================
Improvements:
* [OLMIS-6707](https://openlmis.atlassian.net/browse/OLMIS-6707): Changed fetching FTAPs only for regular Requisition Line Items rather than for all available products.

Bug fixes:
* [OLMIS-6671](https://openlmis.atlassian.net/browse/OLMIS-6671): Changed setNullForCalculatedFields to fix error with saving many requisitions when a template column is hidden.

8.2.0 / 2019-10-17
==================

New functionality:
* [OLMIS-6413](https://openlmis.atlassian.net/browse/OLMIS-6413): Added support for orderable versioning
* [OLMIS-6414](https://openlmis.atlassian.net/browse/OLMIS-6414): Added support for ftap versioning
* [OLMIS-6415](https://openlmis.atlassian.net/browse/OLMIS-6415): Added new versions of requisition endpoints for better support orderable and FTAP versioning
* [OLMIS-6558](https://openlmis.atlassian.net/browse/OLMIS-6558): Add new environment variable - PUBLIC_URL and use to for email generated links

Improvements:
* [OLMIS-6332](https://openlmis.atlassian.net/browse/OLMIS-6332): Changed default page size to max integer.
* [OLMIS-6382](https://openlmis.atlassian.net/browse/OLMIS-6382): Removed usage deprecated endpoint `GET api/facilities/supplying` and use instead `GET /api/supplyLines` endpoint.
* [OLMIS-6374](https://openlmis.atlassian.net/browse/OLMIS-6374): Add new profile for audit logging.
* [OLMIS-6408](https://openlmis.atlassian.net/browse/OLMIS-6408): Added pageable validator.
* [OLMIS-6402](https://openlmis.atlassian.net/browse/OLMIS-6402): Renamed versionId field to versionNumber.
* [OLMIS-6619](https://openlmis.atlassian.net/browse/OLMIS-6619): Improved the `GET /api/requisitionsForApproval` endpoint performance
* [OLMIS-6609](https://openlmis.atlassian.net/browse/OLMIS-6609): Improved the `GET /api/requisitions/periodsForInitiate` endpoint performance

Bug fixes:
* [OLMIS-6230](https://openlmis.atlassian.net/browse/OLMIS-6230): Fixed bug with null value of average consumption when column is not selected in template.
* [OLMIS-6379](https://openlmis.atlassian.net/browse/OLMIS-6379): Fixed bug with submit requisition when column is hidden and source is stock cards.
* [OLMIS-6429](https://openlmis.atlassian.net/browse/OLMIS-6429): Marked `GET api/requisitions/submitted` endpoint as deprecated
* [OLMIS-6597](https://openlmis.atlassian.net/browse/OLMIS-6597): Fixed issue with deserializing JSONs that it stores in the jv_snapshot table.

8.0.0 / 2019-05-27
==================

Breaking changes:
* [OLMIS-4373](https://openlmis.atlassian.net/browse/OLMIS-4373): Changed filterBy and filterValue parameters to programId and facilityId in GET /api/requisitionsForConvert endpoint.

New functionality:
* [OLMIS-5138](https://openlmis.atlassian.net/browse/OLMIS-5138): Added `originalRequisition` extended property to Requisition model
* [OLMIS-5142](https://openlmis.atlassian.net/browse/OLMIS-5142): Added ability to split a requisition if there are defined supply partner(s)

Improvements:
* [OLMIS-3773](https://openlmis.atlassian.net/browse/OLMIS-3773): Adjusted facility search by ids and findAll after changing return type to page.
* [OLMIS-5000](https://openlmis.atlassian.net/browse/OLMIS-5000): Refactored /requisitionsForConvert endpoint to improve its performance.
* [OLMIS-5909](https://openlmis.atlassian.net/browse/OLMIS-5909): Adjusted permission checks in the service
  *  a user without correct permission strings but with correct role assignments will be able to go through requisition workflow and find correct requisitions by the search endpoint.
* [OLMIS-4531](https://openlmis.atlassian.net/browse/OLMIS-4531): Added compressing HTTP POST responses.
* [OLMIS-6118](https://openlmis.atlassian.net/browse/OLMIS-6118): Fix performance test for requisition workflow.
* [OLMIS-6118](https://openlmis.atlassian.net/browse/OLMIS-6118): Fix performance test for requisitions batch approval.

Bug fixes:
* [OLMIS-5847](https://openlmis.atlassian.net/browse/OLMIS-5847): Made that requisition template names are case insensitive unique.
* [OLMIS-5514](https://openlmis.atlassian.net/browse/OLMIS-5514): Validate requisition template column labels support UTF-8, not just alphanumeric.
* [OLMIS-5890](https://openlmis.atlassian.net/browse/OLMIS-5890): The product code column is now visible in the requisition print again.
* [OLMIS-5889](https://openlmis.atlassian.net/browse/OLMIS-5889): The product name column is now visible in the requisition print again.
* [OLMIS-5887](https://openlmis.atlassian.net/browse/OLMIS-5887): The unit of issue column is now visible in the requisition print again.
* [OLMIS-5884](https://openlmis.atlassian.net/browse/OLMIS-5884): The maximum stock quantity column is now visible in the requisition print again.
* [OLMIS-5888](https://openlmis.atlassian.net/browse/OLMIS-5888): The calc order qty isa column is now visible in the requisition print again.
* [OLMIS-5886](https://openlmis.atlassian.net/browse/OLMIS-5886): Show `Additional Quantity Required` column in requisition print
* [OLMIS-5885](https://openlmis.atlassian.net/browse/OLMIS-5885): The ideal stock amount column is now visible in the requisition print again.
* [OLMIS-5963](https://openlmis.atlassian.net/browse/OLMIS-5963): The remarks column in requisition print is now only visible if requisition is at least AUTHORIZED.
* [OLMIS-6193](https://openlmis.atlassian.net/browse/OLMIS-6193): Fixed translating messages for requisition status change notifications.
* [OLMIS-6090](https://openlmis.atlassian.net/browse/OLMIS-6090): Fixed convert to order endpoint for facilities with multiple program handled by the same supplying facility.

7.1.0 / 2018-12-12
==================

New functionality:
* [OLMIS-5334](https://openlmis.atlassian.net/browse/OLMIS-5334): Enabled Total and Number of patients added columns for Stock Based Requisitions.
* [OLMIS-5335](https://openlmis.atlassian.net/browse/OLMIS-5335): Enabled Adjusted Consumption column for Stock Based Requisitions.
* [OLMIS-5314](https://openlmis.atlassian.net/browse/OLMIS-5314): Enabled Total and Number of patients added columns for Stock Based Requisitions.
* [OLMIS-5366](https://openlmis.atlassian.net/browse/OLMIS-5366): Enabled Average Consumption column for Stock Based Requisitions.
* [OLMIS-5226](https://openlmis.atlassian.net/browse/OLMIS-5226): Added sort parameter to the /requisitions/requisitionsForApproval endpoint
* [OLMIS-5395](https://openlmis.atlassian.net/browse/OLMIS-5395): Enabled Maximum Stock Quantity and Calculated Order Quantity columns for Stock Based Requisitions.
* [OLMIS-5461](https://openlmis.atlassian.net/browse/OLMIS-5461): Updated Requisitions API to include parameter for modifiedDate

Improvements:
* [OLMIS-4295](https://openlmis.atlassian.net/browse/OLMIS-4295): Updated checkstyle to use newest google style.
* [OLMIS-3078](https://openlmis.atlassian.net/browse/OLMIS-3078): Made Javers log initializer not iterate over all items, only those without logs.
* [OLMIS-4942](https://openlmis.atlassian.net/browse/OLMIS-4942): Added loading currency, number and date settings from properties on startup.
* [OLMIS-4943](https://openlmis.atlassian.net/browse/OLMIS-4943): Fixed Jasper reports to use service locale settings.

Bug fixes:
* [OLMIS-5065](https://openlmis.atlassian.net/browse/OLMIS-5065): Brought back requisition timestamps validation
* [OLMIS-4938](https://openlmis.atlassian.net/browse/OLMIS-4938): Added default definition for skipped column.
* [OLMIS-4968](https://openlmis.atlassian.net/browse/OLMIS-4968): Added column options for Packs To Ship so that the column can be seen only on approval or on all requisition screens
* [OLMIS-5456](https://openlmis.atlassian.net/browse/OLMIS-5456): the `statusChanges` field in the requisition dto will contains the latest entries
* [OLMIS-5494](https://openlmis.atlassian.net/browse/OLMIS-5494): Fixed column names in requisition template available columns.
* [OLMIS-5635](https://openlmis.atlassian.net/browse/OLMIS-5635): Adjusted supervisory node structure

7.0.0 / 2018-08-16
==================

Contract breaking changes:
* [OLMIS-3162](https://openlmis.atlassian.net/browse/OLMIS-3162): Make periodsForInitiate endpoint self-sufficient to determine available actions.

New functionality:
* [OLMIS-4792](https://openlmis.atlassian.net/browse/OLMIS-4792): Make additional options column of requisition template translatable.
* [OLMIS-4681](https://openlmis.atlassian.net/browse/OLMIS-4681): Added additional option and source option for skipped column.
* [OLMIS-4015](https://openlmis.atlassian.net/browse/OLMIS-4015): Added GET /api/availableRequisitionColumns endpoint
  * updated label in bootstrap data for skipped column
  * updated messages
  * fixed displaying messages with params
* [OLMIS-4707](https://openlmis.atlassian.net/browse/OLMIS-4707): Requisition templates now support tags.
* [OLMIS-4053](https://openlmis.atlassian.net/browse/OLMIS-4053): Enabled beginning balance column for stock based requisition templates
  * data would be populated with the stock on hand at the end of the previous requisition period
  * after initialization values cannot be changed
* [OLMIS-4747](https://openlmis.atlassian.net/browse/OLMIS-4747): Populated Total Received Quantity and Total Consumed Quantity columns for Stock Based Requisitions
* [OLMIS-4748](https://openlmis.atlassian.net/browse/OLMIS-4748): Total Losses and Adjustment column is populated from stock cards for Stock Based Requisitions.
* [OLMIS-4760](https://openlmis.atlassian.net/browse/OLMIS-4760): Total Stockout Days column is populated from stock cards for Stock Based Requisitions.
* [OLMIS-4683](https://openlmis.atlassian.net/browse/OLMIS-4683): Carry over skipped column value from previous requisition
* [OLMIS-4958](https://openlmis.atlassian.net/browse/OLMIS-4958): Add a new /requisitions/batchReleases endpoint to release requisitions with or without order.
* [OLMIS-4982](https://openlmis.atlassian.net/browse/OLMIS-4982): Support initiating report-only requisitions.
* [OLMIS-4981](https://openlmis.atlassian.net/browse/OLMIS-4981): Support approving report-only requisitions.
* [OLMIS-4966](https://openlmis.atlassian.net/browse/OLMIS-4966): Add additionalQuantityRequired, a new column on requisition template.

Improvements:
* [OLMIS-4642](https://openlmis.atlassian.net/browse/OLMIS-4642): Added Jenkinsfile
* [OLMIS-3953](https://openlmis.atlassian.net/browse/OLMIS-3953): Disabled sending stock event when the requisition is configured to pull data from stock cards
* [OLMIS-4714](https://openlmis.atlassian.net/browse/OLMIS-4714): Stock based requisitions will now contain a subset of approved products if there are no stock cards for some of them (rather than throwing an error about missing stock card).
* [OLMIS-4728](https://openlmis.atlassian.net/browse/OLMIS-4728): Added optimistic locking for requisition updates, eTags and If-Match header support in PUT /requisitions
* [OLMIS-4935](https://openlmis.atlassian.net/browse/OLMIS-4935): Requisition status change endpoints now are supporting Idempotent-Key header.
* [OLMIS-4905](https://openlmis.atlassian.net/browse/OLMIS-4905): Updated notification service to use v2 endpoint.
* [OLMIS-4876](https://openlmis.atlassian.net/browse/OLMIS-4876): Applied new demo data loading approach

Bug fixes:
* [OLMIS-4581](https://openlmis.atlassian.net/browse/OLMIS-4581): Fixed packs to ship and total cost calculation on Approve step to always use approved quantity.
* [OLMIS-4697](https://openlmis.atlassian.net/browse/OLMIS-4697): Fix Timeliness report: nulls in "Facility type" column
* [OLMIS-3288](https://openlmis.atlassian.net/browse/OLMIS-3288): Fix period select issues in the Timeliness and Reporting Rate reports
* [OLMIS-4639](https://openlmis.atlassian.net/browse/OLMIS-4639): Made Requisition's numberOfMonthsInPeriod field as invariant
* [OLMIS-4768](https://openlmis.atlassian.net/browse/OLMIS-4768): Fixed duplicate status changes check for programs with skip authorization step enabled
* [OLMIS-5242](https://openlmis.atlassian.net/browse/OLMIS-5242): Added supervisory node reset after rejection

6.0.0 / 2018-04-24
==================

Contract breaking changes:
* [OLMIS-4086](https://openlmis.atlassian.net/browse/OLMIS-4086): Support multiple requisition templates per program
  * The requisition template search endpoint has been removed
* [OLMIS-4112](https://openlmis.atlassian.net/browse/OLMIS-4112): Allow adding/removing line items for emergency requisitions
  * if requisition line item will be added/removed from regular requisition, error message will be shown
  * if orderableId field in requisition line item will be changed (for both regular and emergency requisitions), error message will be shown
* [OLMIS-4076](https://openlmis.atlassian.net/browse/OLMIS-4076): The constraint to verify uniqueness of status changes was replaced with a trigger. The status changes do no longer link to previous status change.

New functionality:
* [OLMIS-3917](https://openlmis.atlassian.net/browse/OLMIS-3917): Added stock based Requisition configuration to template.
* [OLMIS-4113](https://openlmis.atlassian.net/browse/OLMIS-4113): Populate available products on initiate for emergency requisitions
* [OLMIS-4114](https://openlmis.atlassian.net/browse/OLMIS-4114): Adjust validations for emergency requisitions
* [OLMIS-4054](https://openlmis.atlassian.net/browse/OLMIS-4054): Stock based requisition: Stock on Hand from stock management
  * Stock on hand requisition column is populated with the stock on hand from Stock.

Bug fixes added in a backwards-compatible manner:
* [OLMIS-3613](https://openlmis.atlassian.net/browse/OLMIS-3613): Updated Fulfillment Order service to use new fulfillment API.
* [OLMIS-3295](https://openlmis.atlassian.net/browse/OLMIS-3295): Updated stock event structure to match new stock management API
* [OLMIS-3135](https://openlmis.atlassian.net/browse/OLMIS-3135): Handle API Key requests.
  * For now all requests are blocked.
* [OLMIS-3778](https://openlmis.atlassian.net/browse/OLMIS-3778): Fixed Requisition service checked rights of a wrong user
* [OLMIS-2695](https://openlmis.atlassian.net/browse/OLMIS-2695): Handle new version of processing period search endpoint
* [OLMIS-3941](https://openlmis.atlassian.net/browse/OLMIS-3941): Snapshot ISA for products on requisition initialization
* [OLMIS-3492](https://openlmis.atlassian.net/browse/OLMIS-3492): Allow to save comment in requisition longer than 255 characters
* [OLMIS-3956](https://openlmis.atlassian.net/browse/OLMIS-3956): Updated Proof of Delivery service to use new fulfillment API
* [OLMIS-4281](https://openlmis.atlassian.net/browse/OLMIS-4281): Updated Orderable service to use new reference data API
* [OLMIS-3513](https://openlmis.atlassian.net/browse/OLMIS-3513): Fixed problem with handling filter options in /api/requisitionsForConvert endpoint:
    * Returns empty page if no facilities/programs were found for given filter
    * If facilities/programs were found returns page of matching approved requisitions.
* [OLMIS-4368](https://openlmis.atlassian.net/browse/OLMIS-4368): Split huge requests to other services into smaller chunks
* [OLMIS-4396](https://openlmis.atlassian.net/browse/OLMIS-4396): Requisition line items pricePerPack will be retrieved from Referencedata from now on, user input is ignored
* [OLMIS-4530](https://openlmis.atlassian.net/browse/OLMIS-4530): Requested quantity is required for emergency requisitions.
* [OLMIS-4490](https://openlmis.atlassian.net/browse/OLMIS-4490): Submit with authorization step skipped now produces status changes for both submit and authorize.

Improvements:
* [OLMIS-3955](https://openlmis.atlassian.net/browse/OLMIS-3955): Renamed PICKING order status to FULFILLING. Removed PICKED and IN_TRANSIT.
* [OLMIS-3925](https://openlmis.atlassian.net/browse/OLMIS-3925): Updated requisition approval process to stop at an intermediate supervisory node that has a supply line for the requisition's program.
* [OLMIS-3930](https://openlmis.atlassian.net/browse/OLMIS-3930): a requisition will be converted to an order if for a supplying facility/program combo locally fulfill flags is set
* [OLMIS-3938](https://openlmis.atlassian.net/browse/OLMIS-3938): Configured ISA column on requisition template.
* [OLMIS-4016](https://openlmis.atlassian.net/browse/OLMIS-4016): Change the way how service find a correct requisition template
  * use both program and facility type IDs
* [OLMIS-4165](https://openlmis.atlassian.net/browse/OLMIS-4165): Changed Order search endpoint and renamed its parameters.
* [OLMIS-4160](https://openlmis.atlassian.net/browse/OLMIS-4160): Added Calculated Order Quantity ISA column to requisition template.
* [OLMIS-4420](https://openlmis.atlassian.net/browse/OLMIS-4420): All requisition endpoint responses now contain created (initiated) date.

5.1.0 / 2017-11-09
==================

Improvements:
* [OLMIS-3544](https://openlmis.atlassian.net/browse/OLMIS-3544): Added sort to requisition search endpoint.
* [OLMIS-3246](https://openlmis.atlassian.net/browse/OLMIS-3246): Added field hidden to stock adjustment reasons.
  * During work on this ticket also added validations for all special reasons from Stock are valid reasons.
* [OLMIS-3233](https://openlmis.atlassian.net/browse/OLMIS-3233): Added ability to delete requisitions with "skipped" status.
* [OLMIS-3351](https://openlmis.atlassian.net/browse/OLMIS-3351): Improve performance of batch retrieveAll.

Bug fixes added in a backwards-compatible manner:
* [OLMIS-3126](https://openlmis.atlassian.net/browse/OLMIS-3126): Fix unable to batch save when skip is disabled in Requisition Template.
* [OLMIS-3215](https://openlmis.atlassian.net/browse/OLMIS-3215): Do not allow for status change (submit/authorize/approve) when period end after today.
* [OLMIS-3076](https://openlmis.atlassian.net/browse/OLMIS-3076): Exclude emergency from previous requisitions, remove regular requisition only if it is newest.
* [OLMIS-3320](https://openlmis.atlassian.net/browse/OLMIS-3320): Improved requisitions for convert endpoint performance.
* [OLMIS-3404](https://openlmis.atlassian.net/browse/OLMIS-3404): Added validation for sending reasons in line item adjustments that are not present on available reason list in requisition.

Improve demo data:
* [OLMIS-3202](https://openlmis.atlassian.net/browse/OLMIS-3202): Modified requisition template for EM program to match Malawi columns.

5.0.0 / 2017-09-01
==================

Contract breaking changes:

* [OLMIS-2612](https://openlmis.atlassian.net/browse/OLMIS-2612): Configuration settings endpoints
(/api/settings) are no longer available. Use environment variables to configure the application.
* [MW-365](https://openlmis.atlassian.net/browse/MW-365): Requisition search endpoints: requisitionsForApproval and requisitionsForConvert will now return smaller basic dtos.
* [OLMIS-2833](https://openlmis.atlassian.net/browse/OLMIS-2833): Added date physical stock count completed to Requisition
* [OLMIS-2671](https://openlmis.atlassian.net/browse/OLMIS-2671): Stock Management service is now required by Requisition
* [OLMIS-2694](https://openlmis.atlassian.net/browse/OLMIS-2694): Changed Requisition adjustment reasons to come from Stock Service
* [OLMIS-2898](https://openlmis.atlassian.net/browse/OLMIS-2898): Requisition search endpoint takes from/to parameters as dates without time part.
* [OLMIS-2830](https://openlmis.atlassian.net/browse/OLMIS-2830): As of this version. Requisition now uses Stock Management as the source for adjustment reasons, moreover it stores snapshots of these available reasons during initiation. **Important**: in order to migrate from older versions, running this migration is required - https://github.com/OpenLMIS/openlmis-adjustment-reason-migration

New functionality added in a backwards-compatible manner:

* [OLMIS-2709](https://openlmis.atlassian.net/browse/OLMIS-2709): Changed ReferenceData facility service search endpoint to use smaller dto.
* The /requisitions/requisitionsForConvert endpoint accepts several sortBy parameters. Data returned by the endpoint will be sorted by those parameters in order of occurrence. By defaults data will be sorted by emergency flag and program name.
* [OLMIS-2928](https://openlmis.atlassian.net/browse/OLMIS-2928): Introduced new batch endpoints, that allow retrieval and approval of several requisitions at once. This also refactored the error handling.

Bug fixes added in a backwards-compatible manner:
* [OLMIS-2788](https://openlmis.atlassian.net/browse/OLMIS-2788): Fixed print requisition.
* [OLMIS-2747](https://openlmis.atlassian.net/browse/OLMIS-2747): Fixed bug preventing user from being able to re-initiate a requisition after being removed, when there's already a requisition for next period.
* [OLMIS-2871](https://openlmis.atlassian.net/browse/OLMIS-2871): The service now uses an Authorization header instead of an access_token request parameter when communicating with other services.
* [OLMIS-2534](https://openlmis.atlassian.net/browse/OLMIS-2534): Fixed potential huge performance issue. The javers log initializer will not retrieve all domain objects at once if a repository implemenets [PagingAndSortingRepository](https://docs.spring.io/spring-data/commons/docs/current/api/org/springframework/data/repository/PagingAndSortingRepository.html)
* [OLMIS-3008](https://openlmis.atlassian.net/browse/OLMIS-3008): Add correct error message when trying to convert requisition to an order with approved quantity disabled in the the requisition template.
* [OLMIS-2908](https://openlmis.atlassian.net/browse/OLMIS-2908): Added a unique partial index on requisitions, which prevents creation of requisitions which have the same facility, program and processing period while being a non-emergency requsition. This is now enforced by the database, not only the application logic.
* [OLMIS-3019](https://openlmis.atlassian.net/browse/OLMIS-3019): Removed clearance of beginning balance and price per pack fields from skipped line items while authorizing.
* [OLMIS-2911](https://openlmis.atlassian.net/browse/OLMIS-2911): Added HTTP method parameter to jasper template parameter object.
* [OLMIS-2681](https://openlmis.atlassian.net/browse/OLMIS-2681): Added profiling to requisition search endpoint, also it is using db pagination now.

4.0.0 / 2017-06-23
==================

Contract breaking changes:

* [OLMIS-2566](https://openlmis.atlassian.net/browse/OLMIS-2566): Requisition search endpoint will
now return new, smaller Dto object, which only contains basic information about the requisition,
processing period, program and facility.
* [OLMIS-2533](https://openlmis.atlassian.net/browse/OLMIS-2533): Requisition endpoints: initiate,
update, submit, authorize, approve, requisitionsForConvert will now return new, smaller Dto object,
which only contains basic information required for API client.
* [MW-305](https://openlmis.atlassian.net/browse/MW-305): Requisition submit, skip, reject, approve and authorize endpoints now are returning smaller basic dto.
* Added new REJECTED status which is basically the same as INITIATED.

New functionality added in a backwards-compatible manner:

* [OLMIS-2664](https://openlmis.atlassian.net/browse/OLMIS-2664): Requisition reject endpoint saves status messages.
* [OLMIS-2611](https://openlmis.atlassian.net/browse/OLMIS-2611): Added using locale from env file.

Bug fixes added in a backwards-compatible manner:
* [OLMIS-2551](https://openlmis.atlassian.net/browse/OLMIS-2551): Use batch order creation
endpoint from fulfilment when converting requisitions to orders - all orders are created in a
single transaction.
* [OLMIS-2596](https://openlmis.atlassian.net/browse/OLMIS-2596): Update endpoint returns better message when cannot update
* [OLMIS-2280](https://openlmis.atlassian.net/browse/OLMIS-2280): Updated Referencedata Supervisory Node service to use paginated search endpoint.

Performance improvements added in a backwards-compatible manner:
* [MW-310](https://openlmis.atlassian.net/browse/MW-310): Improve performance of RequisitionDto by utilizing search orderables
* [MW-309](https://openlmis.atlassian.net/browse/MW-309): Improve performance of Requisition initialize
* Improve performance of view right checking for multiple requisitions

3.1.4 / 2017-05-26
===================

Bug fixes added in a backwards-compatible manner:
* [OLMIS-1696](https://openlmis.atlassian.net/browse/OLMIS-1696): Update to accomodate referencedata 6.0.0 API change
  * Use pagination for calls to referencedata getAll Orderables
* [OLMIS-2480](https://openlmis.atlassian.net/browse/OLMIS-2480): Lowered values of maxPeriodsOfStock
* [OLMIS-2530](https://openlmis.atlassian.net/browse/OLMIS-2530): Fixed pagination on requisition search endpoint.
* [OLMIS-2491](https://openlmis.atlassian.net/browse/OLMIS-2491): Configuration Settings were moved to Transifex messages and environment variables.
* [OLMIS-2514](https://openlmis.atlassian.net/browse/OLMIS-2514): Added check for supervisory node and program role assignment to approve/reject endpoints.

3.1.3 / 2017-05-17
===================

Bug fixes added in a backwards-compatible manner:

* [OLMIS-2407](https://openlmis.atlassian.net/browse/OLMIS-2407): Add due date label on the reporting rate report
* [Pull Request 23](https://github.com/OpenLMIS/openlmis-requisition/pull/23): Skip permission checks and add unit tests for service level tokens

3.1.2 / 2017-05-08
===================

New functionality added in a backwards-compatible manner:

* [OLMIS-2420](https://openlmis.atlassian.net/browse/OLMIS-2420): Requisition forApproval endpoint uses optional program filter

Bug fixes and performance improvements which are backwards-compatible:

* [OLMIS-2307](https://openlmis.atlassian.net/browse/OLMIS-2307): Requisition Beginning Balance includes SOH and Approved Quantity
* [OLMIS-2218](https://openlmis.atlassian.net/browse/OLMIS-2218): Requisition column Total Losses and Adjustments validation
* [OLMIS-2302](https://openlmis.atlassian.net/browse/OLMIS-2302): Requisition template configuration validation for Adjusted Consumption
* [OLMIS-2158](https://openlmis.atlassian.net/browse/OLMIS-2158): Requisition print out populating quantities and display order
* [OLMIS-2322](https://openlmis.atlassian.net/browse/OLMIS-2322): Notify warehouse clerk when a requisition is ready to convert to order
* [OLMIS-2314](https://openlmis.atlassian.net/browse/OLMIS-2314): Creator should not be notified when requisition is submitted
* [OLMIS-2330](https://openlmis.atlassian.net/browse/OLMIS-2330): Update Reporting Rate report
  * Display additional raw counts on the report, as well as the date generated and due date.
* [OLMIS-2320](https://openlmis.atlassian.net/browse/OLMIS-2320): Cannot create new FacilityTypeApprovedProduct
* [OLMIS-2155](https://openlmis.atlassian.net/browse/OLMIS-2155): Performance issue with custom ZonedDateTimeAttributeConverter

Dev and tooling updates made in a backwards-compatible manner:

* [OLMIS-1972](https://openlmis.atlassian.net/browse/OLMIS-1972): Update Postgres from 9.4 to 9.6
  * This upgrade will apply automatically and all data will migrate.
* [OLMIS-2330](https://openlmis.atlassian.net/browse/OLMIS-2330): Update for ReferenceData pagination of the facility search endpoint
* Update [Docker Dev Image](https://github.com/OpenLMIS/docker-dev) for builds from v1 to v2
  * Moves the sync_transifex.sh script out of each service and into the Docker Dev Image.
* Add possibility to define filter options in a Jasper template parameter
  * Thanks to the Malawi implementation team for [Pull Request 18](https://github.com/OpenLMIS/openlmis-requisition/pull/18)
    and [Pull Request 21](https://github.com/OpenLMIS/openlmis-requisition/pull/21).
* Improve demo data
  * Add multiple authorized requisitions for the Family Planning program Mar2017 period.
  * Add stock adjustments into requisition line items in demo data.
  * Note: New demo data is available for fresh installations, but it will not be loaded into any system in Production mode.

3.1.1 / 2017-03-30
==================

* [OLMIS-2220](https://openlmis.atlassian.net/browse/OLMIS-2220): Changed the request body parameter for searching facilities from zone to zoneId, to match openlmis-referencedata 4.0.0 API changes.
* [OLMIS-2203](https://openlmis.atlassian.net/browse/OLMIS-2203): Changed requisition template column for Adjusted Consumption to be initially visible in demo data.
* [OLMIS-2237](https://openlmis.atlassian.net/browse/OLMIS-2237): Changed querying facilities on /api/requisitions/requisitionsForConvert endpoint to not search by empty filter values (which would produce bad request response in openlmis-referencedata 4.0.0) - in this case, the service queries for all facilities.

3.1.0 / 2017-03-28
==================

New functionality added in a backwards-compatible manner:

* [OLMIS-1911](https://openlmis.atlassian.net/browse/OLMIS-1911): Calculated Order and Requested Quantity Configuration
  * If Calculated Order Quantity field is displayed on the requisition form, then Requested Quantity
    is not required. If a Requested Quantity is entered, then Requested Quantity Explanation is required.
    If Calculated Order Quantity field is not displayed, then Requested Quantity is required, but
    Requested Quantity Explanation is not required. This is a behavior change from 3.0.0.
* [OLMIS-1989](https://openlmis.atlassian.net/browse/OLMIS-1989): Approved Quantity Autopopulate
  * The Approved Quantity field on the requisition form will initially default to an auto-populated
    value. This value comes from the Calculated Order Quantity field - if it is displayed on the form -
    otherwise the value comes from the Requested Quantity field. Users can still change the Approved
    Quantity, but having this initial default value helps save time.
* [OLMIS-1395](https://openlmis.atlassian.net/browse/OLMIS-1395): Add status changes to OrderDto
  * This adds one property, List<StatusChangeDto> statusChanges, into the OrderDTO. It makes the status change
    history available for other services, such as when fulfillment converts a requisition to an order.
* [OLMIS-1942](https://openlmis.atlassian.net/browse/OLMIS-1942): Add pagination to requisitions for approval endpoint
  * The /requisitions/requisitionsForApproval endpoint now returns paginated results. This API change
    was contributed by developers working on the openlmis-requisition-refUI, so the latest version
    of the UI is already compatible.

Bug fixes, security and performance improvements, also backwards-compatible:

* [OLMIS-1940](https://openlmis.atlassian.net/browse/OLMIS-1940): Refactor Requisition to use new status changes not Javers
  * For performance reasons, we reduced the role of Javers in requisition searches. We create a new status_changes table
    to track changes more efficiently. This change does not alter the statusChanges data returned by the API.
    However, this change does not include an automated migration. Therefore any production data that contains requisitions
    would have an empty status_changes table and empty statusChanges JSON object for historical requisitions after applying this update.
* [OLMIS-2039](https://openlmis.atlassian.net/browse/OLMIS-2039): Fix Jasper Reports HTML chart display
* [OLMIS-2116](https://openlmis.atlassian.net/browse/OLMIS-2116): Fix item count in paginated requisition endpoints
* [OLMIS-2195](https://openlmis.atlassian.net/browse/OLMIS-2195): Fix backend validations for Total Stockout Days and Adjusted Consumption
  * If Adjusted Consumption (Column N) is not displayed, then Total Stockout Days (Column X) is not required.
    If Adjusted Consumption (N) is displayed, then Total Stockout Days (X) is required.
    If Average Consumption (P) is displayed, Adjusted Consumption (N) is Required.
* [OLMIS-1773](https://openlmis.atlassian.net/browse/OLMIS-1773): Update for ReferenceData renaming Orderable packSize and name for GS1
* [OLMIS-2202](https://openlmis.atlassian.net/browse/OLMIS-2202): Update for ReferenceData pagination of the users search endpoint
* [OLMIS-2182](https://openlmis.atlassian.net/browse/OLMIS-2182): Check permission for delete a requisition
  * This is a fix for the permissions required to delete a requisition in each status. When in the INITIATED state, we check
    for both REQUISITION_INITIATE and REQUISITION_DELETE rights. If user has both of those rights, allow them to delete.
    When in the SUBMITTED state, we check for both REQUISITION_AUTHORIZE and REQUISITION_DELETE rights. If a user has both of
    those rights, allow them to delete. When in any other state (AUTHORIZED, APPROVED, RELEASED), delete is not allowed at all.
* [OLMIS-2027](https://openlmis.atlassian.net/browse/OLMIS-2027): Add unit tests for Timeliness and Reporting Rate reports
* [OLMIS-2081](https://openlmis.atlassian.net/browse/OLMIS-2081): Add missing unit tests for RequisitionTemplateValidator
* [OLMIS-1142](https://openlmis.atlassian.net/browse/OLMIS-1142): Migrated service to Spring Boot 1.4.1
  * Fix Jackson configuration on AvailableRequisitionColumnOption
  * Fix column types (UUID) for some entities
    * This change includes two database migrations that run automatically to change column type without data loss.
  * Refractor controller integration tests, extract common logics into utility methods
* [OLMIS-2112](https://openlmis.atlassian.net/browse/OLMIS-2112): Refactor web layer for requisition templates to use DTOs, not domain objects
* [OLMIS-2139](https://openlmis.atlassian.net/browse/OLMIS-2139): Fix demo data to initiate Family Planning requisition
* [OLMIS-2082](https://openlmis.atlassian.net/browse/OLMIS-2082): Fix demo data requisition template skip instructions
* [OLMIS-1917](https://openlmis.atlassian.net/browse/OLMIS-1917): Fix demo data for requisition statusChanges missing a user ID
  * Also handle null user names in requisition report gracefully
* Add unit test for RequisitionReportDtoBuilder
* Correct JasperReportsViewService's javadoc

3.0.0 / 2017-03-01
==================

* Released openlmis-requisition 3.0.0 as part of openlmis-ref-distro 3.0.0. See [3.0.0 Release Notes](https://openlmis.atlassian.net/wiki/display/OP/3.0.0+Release+Notes).
 * This was the first stable release of openlmis-requisition. It builds on the code, patterns, and lessons learned from OpenLMIS 1 and 2.
