package org.openlmis.settings.repository;

import org.openlmis.referencedata.repository.ReferenceDataRepository;
import org.openlmis.settings.domain.ConfigurationSetting;
import org.springframework.data.rest.core.annotation.RestResource;

public interface ConfigurationSettingRepository
    extends ReferenceDataRepository<ConfigurationSetting, String> {

  @Override
  @RestResource
  <S extends ConfigurationSetting> S save(S entity);

  @Override
  @RestResource
  <S extends ConfigurationSetting> Iterable<S> save(Iterable<S> entities);
}
