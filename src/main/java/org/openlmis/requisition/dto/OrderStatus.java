package org.openlmis.requisition.dto;

public enum OrderStatus {
  ORDERED,
  IN_TRANSIT,
  PICKING,
  PICKED,
  SHIPPED,
  RECEIVED,
  TRANSFER_FAILED,
  IN_ROUTE,
  READY_TO_PACK
}
