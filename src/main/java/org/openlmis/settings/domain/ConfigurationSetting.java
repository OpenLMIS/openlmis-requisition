package org.openlmis.settings.domain;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "settings")
@NoArgsConstructor
@AllArgsConstructor
public class ConfigurationSetting {

  @Id
  @Getter
  @Setter
  private String key;

  @Column(nullable = false)
  @Getter
  @Setter
  private String value;
}
