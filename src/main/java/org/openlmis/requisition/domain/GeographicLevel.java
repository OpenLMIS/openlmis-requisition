package org.openlmis.requisition.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;

@Entity
@Table(name = "geographic_levels")
@NoArgsConstructor
public class GeographicLevel {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Getter
    @Setter
    private Integer id;

    @Column(nullable = false, unique = true, columnDefinition = "text")
    @Getter
    @Setter
    private String code;

    @Column(columnDefinition = "text")
    @Getter
    @Setter
    private String name;

    @Column(nullable = false)
    @Getter
    @Setter
    private Integer levelNumber;
}
