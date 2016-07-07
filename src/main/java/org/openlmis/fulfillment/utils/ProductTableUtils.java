package org.openlmis.fulfillment.utils;

import com.itextpdf.io.font.FontConstants;
import com.itextpdf.kernel.color.Color;
import com.itextpdf.kernel.color.DeviceCmyk;
import com.itextpdf.kernel.font.PdfFont;
import com.itextpdf.kernel.font.PdfFontFactory;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.element.Cell;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.element.Text;
import com.itextpdf.layout.property.TextAlignment;
import org.openlmis.fulfillment.domain.ProofOfDelivery;
import org.openlmis.fulfillment.domain.ProofOfDeliveryLine;
import org.openlmis.product.domain.Product;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class ProductTableUtils {

    private static final Color BLUE_COLOR = new DeviceCmyk(0.445f, 0.0546f, 0, 0.0667f);

    public ProductTableUtils(Document document, ProofOfDelivery proofOfDelivery)
            throws IOException {
        Paragraph products = new Paragraph(
                "Product(s)").setTextAlignment(TextAlignment.LEFT).setFontSize(16);
        document.add(products);
        float[] cellsProportionInTable = new float[]
                {1, 2, 4, 1.5f, 1.5f, 1.5f, 1.5f, 1.5f, 1.5f, 5};
        Table productsTable = new Table(cellsProportionInTable);
        productsTable.setWidthPercent(100);
        addHeaderCellsToProductsTable(productsTable);

        if (proofOfDelivery.getProfOfDeliveryLineItems() != null) {
            Map<String, List<ProofOfDeliveryLine>> orderProofofDeliveryLinesGrouped =
                    proofOfDelivery.getProfOfDeliveryLineItems().stream().collect(
                            Collectors.groupingBy(w ->
                                    w.getOrderLine().getProduct().getProductCategory().getName()));
            Iterator<String> iterator = orderProofofDeliveryLinesGrouped.keySet().iterator();
            while (iterator.hasNext()) {
                String key = iterator.next();
                Paragraph categoryParagraph = new Paragraph(key);
                Cell categoryCell = new Cell(1, 10).add(categoryParagraph);
                categoryCell.setBackgroundColor(BLUE_COLOR);
                productsTable.addCell(categoryCell);
                List<ProofOfDeliveryLine> valList = orderProofofDeliveryLinesGrouped.get(key);
                for (ProofOfDeliveryLine item : valList) {
                    addCellsToProductsTable(productsTable, item);
                }
            }
        }
        productsTable.setTextAlignment(TextAlignment.CENTER);
        document.add(productsTable);
    }

    private void addCellsToProductsTable(Table productsTable,
                                         ProofOfDeliveryLine item) throws IOException {
        Product currentProduct = item.getOrderLine().getProduct();
        Boolean fullSupply = currentProduct.getFullSupply();
        Cell cell = getIcon(fullSupply);
        productsTable.addCell(cell);
        productsTable.addCell(new Cell().add(new Paragraph(currentProduct.getCode())));
        productsTable.addCell(new Cell().add(new Paragraph(currentProduct.getPrimaryName())));
        productsTable.addCell(new Cell().add(new Paragraph(currentProduct.getDispensingUnit())));
        String packToShip = "";
        if (item.getPackToShip() != null) {
            packToShip = item.getPackToShip().toString();
        }
        String quantityShipped = "";
        if (item.getQuantityShipped() != null) {
            quantityShipped = item.getQuantityShipped().toString();
        }
        String quantityReceived = "";
        if (item.getQuantityReceived() != null) {
            quantityReceived = item.getQuantityReceived().toString();
        }
        String quantityReturned = "";
        if (item.getQuantityReturned() != null) {
            quantityReturned = item.getQuantityReturned().toString();
        }
        String replacedProductCode = "";
        if (item.getReplacedProductCode() != null) {
            replacedProductCode = item.getReplacedProductCode();
        }
        String notes = "";
        if (item.getNotes() != null) {
            notes = item.getNotes();
        }
        productsTable.addCell(new Cell().add(new Paragraph(packToShip)));
        productsTable.addCell(new Cell().add(new Paragraph(quantityShipped)));
        productsTable.addCell(new Cell().add(new Paragraph(quantityReceived)));
        productsTable.addCell(new Cell().add(new Paragraph(quantityReturned)));
        productsTable.addCell(new Cell().add(new Paragraph(replacedProductCode)));
        productsTable.addCell(new Cell().add(new Paragraph(notes)));
    }

    private Cell getIcon(Boolean fullSuplay) throws IOException {
        PdfFont zapfdingbats = PdfFontFactory.createFont(FontConstants.ZAPFDINGBATS);
        Cell cell = new Cell();
        if (fullSuplay == true) {
            cell.add(new Paragraph(new Text("4").setFont(zapfdingbats).setFontSize(14)));
        } else {
            cell.add(new Paragraph(new Text("5").setFont(zapfdingbats).setFontSize(14)));
        }
        return cell;
    }

    private void addHeaderCellsToProductsTable(Table productsTable) {
        productsTable.addHeaderCell(new Cell().add(new Paragraph(
                "Full\n Supply")).setBackgroundColor(Color.LIGHT_GRAY));
        productsTable.addHeaderCell(new Cell().add(new Paragraph(
                "Product\n Code")).setBackgroundColor(Color.LIGHT_GRAY));
        productsTable.addHeaderCell(new Cell().add(new Paragraph(
                "Product\n Name")).setBackgroundColor(Color.LIGHT_GRAY));
        productsTable.addHeaderCell(new Cell().add(new Paragraph(
                "Unit\n of\n Issue")).setBackgroundColor(Color.LIGHT_GRAY));
        productsTable.addHeaderCell(new Cell().add(new Paragraph(
                "Packs\n to\n Ship")).setBackgroundColor(Color.LIGHT_GRAY));
        productsTable.addHeaderCell(new Cell().add(new Paragraph(
                "Quantity\n Shipped")).setBackgroundColor(Color.LIGHT_GRAY));
        productsTable.addHeaderCell(new Cell().add(new Paragraph(
                "Quantity\n Received")).setBackgroundColor(Color.LIGHT_GRAY));
        productsTable.addHeaderCell(new Cell().add(new Paragraph(
                "Quantity\n Returned")).setBackgroundColor(Color.LIGHT_GRAY));
        productsTable.addHeaderCell(new Cell().add(new Paragraph(
                "Replaced\n Product\n Code")).setBackgroundColor(Color.LIGHT_GRAY));
        productsTable.addHeaderCell(new Cell().add(new Paragraph(
                "Notes")).setBackgroundColor(Color.LIGHT_GRAY));
    }
}
