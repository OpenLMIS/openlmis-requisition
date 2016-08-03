package org.openlmis.referencedata.domain;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.Set;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.OneToMany;
import javax.persistence.Table;

@Entity
@Table(name = "stockInventories", schema = "referencedata")
@NoArgsConstructor
public class StockInventory extends BaseEntity {

  @Column(nullable = false)
  @Getter
  @Setter
  private String name;

  @OneToMany(mappedBy = "stockInventory", cascade = CascadeType.REMOVE)
  @Getter
  @Setter
  @JsonIdentityInfo(
          generator = ObjectIdGenerators.IntSequenceGenerator.class,
          property = "@stocksId")
  private Set<Stock> stocks;
}
