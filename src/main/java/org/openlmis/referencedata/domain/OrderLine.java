package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;

@Entity
@Table(name = "order_lines")
@NoArgsConstructor
public class OrderLine {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Getter
    @Setter
    private Integer id;

    @ManyToOne
    @JoinColumn(name = "orderId", nullable = false)
    @Getter
    @Setter
    private Order order;

    @ManyToOne
    @JoinColumn(name = "productId", nullable = false)
    @Getter
    @Setter
    private Product product;

    @Column(nullable = false)
    @Getter
    @Setter
    private Long orderedQuantity;
}
