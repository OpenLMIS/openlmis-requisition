package org.openlmis.fulfillment.utils;

import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.kernel.pdf.canvas.draw.SolidLine;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.border.Border;
import com.itextpdf.layout.element.Cell;
import com.itextpdf.layout.element.LineSeparator;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.property.TextAlignment;
import org.openlmis.fulfillment.domain.ProofOfDeliveryLine;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.requisition.domain.Requisition;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;

public class ProofOfDeliveryPdfBuilder extends PdfViewGenerator {

  private static final DateFormat DATE_FORMAT = new SimpleDateFormat("dd MM yyyy");

  @Override
  protected void buildPdfDocument(Map<String, Object> model, Document document, PdfWriter writer,
                                  HttpServletRequest request, HttpServletResponse response)
      throws Exception {

    Map<Requisition, ProofOfDelivery> orderProofOfDeliveries =
        (Map<Requisition, ProofOfDelivery>) model.get("orderProofOfDeliveries");
    ProofOfDelivery proofOfDelivery =
        orderProofOfDeliveries.values().iterator().next();
    Requisition requisition = orderProofOfDeliveries.keySet().iterator().next();
    addHeading(document, proofOfDelivery);
    addFirstSeparator(document);
    new OrderTableUtils(document, proofOfDelivery, requisition);
    addBreakLine(document);
    new ProductTableUtils(document, proofOfDelivery);
    addBreakLine(document);
    addSummary(document, proofOfDelivery);
    document.close();
  }

  private void addHeading(Document document, ProofOfDelivery proofOfDelivery) {
    Table headingTable = new Table(2);
    headingTable.setWidthPercent(100);

    String programName = "";
    if (proofOfDelivery.getOrder().getProgram().getName() != null) {
      programName = proofOfDelivery.getOrder().getProgram().getName();
    }
    Paragraph program = new Paragraph("Proof of Delivery for " +
        programName + "\t").setTextAlignment(TextAlignment.LEFT);
    headingTable.addCell(new Cell().add(program).setBorder(Border.NO_BORDER));

    Paragraph actualDate = new Paragraph(DATE_FORMAT.format(
        new Date())).setTextAlignment(TextAlignment.RIGHT);
    headingTable.addCell(new Cell().add(actualDate).setBorder(Border.NO_BORDER));

    document.add(headingTable);
  }

  private void addFirstSeparator(Document document) {
    SolidLine l = new SolidLine();
    document.add(new LineSeparator(l));
    document.add(new Paragraph("\n"));
  }

  private void addBreakLine(Document document) {
    document.add(new Paragraph("\n"));
  }

  private void addSummary(Document document, ProofOfDelivery proofOfDelivery) {
    Paragraph summary = new Paragraph(
        "Summary").setTextAlignment(TextAlignment.LEFT).setFontSize(16);
    document.add(summary);
    Long totalShipped = new Long(0);
    Long totalReceived = new Long(0);
    Long totalReturned = new Long(0);
    for (ProofOfDeliveryLine currentItem :
        proofOfDelivery.getProfOfDeliveryLineItems()) {
      if (currentItem.getQuantityShipped() != null) {
        totalShipped += currentItem.getQuantityShipped();
      }
      if (currentItem.getQuantityReceived() != null) {
        totalReceived += currentItem.getQuantityReceived();
      }
      if (currentItem.getQuantityReturned() != null) {
        totalReturned += currentItem.getQuantityReturned();
      }
    }
    Paragraph totalShippedPacks = new Paragraph("Total Shipped Packs: " + totalShipped);
    Paragraph totalReceivedPacks = new Paragraph("Total Received Packs: " + totalReceived);
    Paragraph totalReturnedPacks = new Paragraph("Total Returned Packs: " + totalReturned);
    String deliveredBy = "";
    if (proofOfDelivery.getDeliveredBy() != null) {
      deliveredBy = proofOfDelivery.getDeliveredBy();
    }
    Paragraph deliveredByParagraph = new Paragraph("Delivered By: " + deliveredBy);
    String receivedBy = "";
    if (proofOfDelivery.getReceivedBy() != null) {
      receivedBy = proofOfDelivery.getReceivedBy();
    }
    String receivedDate = "";
    if (proofOfDelivery.getReceivedDate() != null) {
      receivedDate = DATE_FORMAT.format(proofOfDelivery.getReceivedDate());
    }
    Paragraph receivedByParagraph = new Paragraph("Received By: " + receivedBy);
    Paragraph receivedDateParagraph = new Paragraph("Received Date: " + receivedDate);
    addSummaryTable(document, totalShippedPacks, totalReceivedPacks, totalReturnedPacks,
        deliveredByParagraph, receivedByParagraph, receivedDateParagraph);
  }

  private void addSummaryTable(Document document, Paragraph totalShippedPacks, Paragraph
      totalReceivedPacks, Paragraph totalReturnedPacks, Paragraph
                                   deliveredBy, Paragraph receivedBy, Paragraph receivedDate) {
    Table summaryTable = new Table(2);
    summaryTable.setWidthPercent(100);
    summaryTable.addCell(new Cell().add(totalShippedPacks).setBorder(Border.NO_BORDER));
    summaryTable.addCell(new Cell().add(deliveredBy).setBorder(Border.NO_BORDER));
    summaryTable.addCell(new Cell().add(totalReceivedPacks).setBorder(Border.NO_BORDER));
    summaryTable.addCell(new Cell().add(receivedBy).setBorder(Border.NO_BORDER));
    summaryTable.addCell(new Cell().add(totalReturnedPacks).setBorder(Border.NO_BORDER));
    summaryTable.addCell(new Cell().add(receivedDate).setBorder(Border.NO_BORDER));
    document.add(summaryTable);
  }
}
