5.0.0 / WIP
==================

Contract breaking changes:

* [OLMIS-2612](https://openlmis.atlassian.net/browse/OLMIS-2612): Configuration settings endpoints
(/api/settings) are no longer available. Use environment variables to configure the application.
* [MW-365](https://openlmis.atlassian.net/browse/MW-365): Requisition search endpoints: requisitionsForApproval and requisitionsForConvert will now return smaller basic dtos.

New functionality added in a backwards-compatible manner:

* [OLMIS-2709](https://openlmis.atlassian.net/browse/OLMIS-2709): Changed ReferenceData facility service search endpoint to use smaller dto.

Bug fixes added in a backwards-compatible manner:
* [OLMIS-2788](https://openlmis.atlassian.net/browse/OLMIS-2788): Fixed print requisition.

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
