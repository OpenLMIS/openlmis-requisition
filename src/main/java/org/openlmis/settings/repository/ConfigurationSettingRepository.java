package org.openlmis.settings.repository;

import org.openlmis.referencedata.repository.ReferenceDataRepository;
import org.openlmis.settings.domain.ConfigurationSetting;

public interface ConfigurationSettingRepository
    extends ReferenceDataRepository<ConfigurationSetting, String> {
}
