package org.openlmis.requisition.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;

@Entity
@Table(name = "users")
@NoArgsConstructor
public class User {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Getter
    @Setter
    private Integer id;

    @Column(nullable = false, columnDefinition = "text")
    @Getter
    @Setter
    private String username;

    @Column(nullable = false, columnDefinition = "text DEFAULT 'not-in-use'::text")
    @Setter
    private String password;

    @Column(nullable = false, columnDefinition = "text")
    @Getter
    @Setter
    private String firstName;

    @Column(nullable = false, columnDefinition = "text")
    @Getter
    @Setter
    private String lastName;

    @ManyToOne
    @JoinColumn(name = "supervisorid")
    private User supervisor;

    @ManyToOne
    @JoinColumn(name = "facilityid")
    private Facility homeFacility;

    @Column(columnDefinition = "boolean DEFAULT false")
    @Getter
    @Setter
    private Boolean verified;

    @Column(columnDefinition = "boolean DEFAULT false")
    @Getter
    @Setter
    private Boolean active;
}
