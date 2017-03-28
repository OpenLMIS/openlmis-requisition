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
