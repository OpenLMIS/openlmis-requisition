/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

package org.openlmis.requisition;

import org.javers.core.Javers;
import org.javers.core.MappingStyle;
import org.javers.core.diff.ListCompareAlgorithm;
import org.javers.hibernate.integration.HibernateUnproxyObjectAccessHook;
import org.javers.repository.sql.ConnectionProvider;
import org.javers.repository.sql.DialectName;
import org.javers.repository.sql.JaversSqlRepository;
import org.javers.repository.sql.SqlRepositoryBuilder;
import org.javers.spring.auditable.AuthorProvider;
import org.javers.spring.boot.sql.JaversProperties;
import org.javers.spring.jpa.TransactionalJaversBuilder;
import org.openlmis.requisition.domain.BaseEntity;
import org.openlmis.requisition.i18n.ExposedMessageSourceImpl;
import org.openlmis.requisition.security.UserNameProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.flyway.FlywayMigrationStrategy;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ImportResource;
import org.springframework.context.annotation.Profile;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.web.servlet.LocaleResolver;
import org.springframework.web.servlet.i18n.CookieLocaleResolver;
import java.time.Clock;
import java.time.ZoneId;
import java.util.Locale;

@SpringBootApplication
@ImportResource("applicationContext.xml")
@EntityScan(basePackageClasses = {BaseEntity.class})
@EnableScheduling
public class Application {
  private Logger logger = LoggerFactory.getLogger(Application.class);

  @Value("${defaultLocale}")
  private Locale locale;

  @Autowired
  private DialectName dialectName;

  @Autowired
  private JaversProperties javersProperties;

  @Value("${spring.jpa.properties.hibernate.default_schema}")
  private String preferredSchema;

  @Value("${time.zoneId}")
  private String timeZoneId;

  @Autowired
  private JdbcTemplate template;

  @Value("${db.clustering.enabled}")
  private boolean dbClusteringEnabled;

  public static void main(String[] args) {
    SpringApplication.run(Application.class, args);
  }

  /**
   * Creates new LocaleResolver.
   *
   * @return Created LocalResolver.
   */
  @Bean
  public LocaleResolver localeResolver() {
    CookieLocaleResolver lr = new CookieLocaleResolver();
    lr.setCookieName("lang");
    lr.setDefaultLocale(locale);
    return lr;
  }

  /**
   * Creates new MessageSource.
   *
   * @return Created MessageSource.
   */
  @Bean
  public ExposedMessageSourceImpl messageSource() {
    ExposedMessageSourceImpl messageSource = new ExposedMessageSourceImpl();
    messageSource.setBasename("classpath:messages");
    messageSource.setDefaultEncoding("UTF-8");
    messageSource.setUseCodeAsDefaultMessage(true);
    return messageSource;
  }

  /**
   * Create and return a UserNameProvider. By default, if we didn't do so, an instance of
   * SpringSecurityAuthorProvider would automatically be created and returned instead.
   */
  @Bean
  public AuthorProvider authorProvider() {
    return new UserNameProvider();
  }


  /**
   * Create and return an instance of JaVers precisely configured as necessary.
   * This is particularly helpful for getting JaVers to create and use tables
   * within a particular schema (specified via the withSchema method).
   *
   * @See <a href="https://github.com/javers/javers/blob/master/javers-spring-boot-starter-sql/src
   * /main/java/org/javers/spring/boot/sql/JaversSqlAutoConfiguration.java">
   * JaversSqlAutoConfiguration.java</a> for the default configuration upon which this code is based
   */
  @Bean
  public Javers javersProvidor(ConnectionProvider connectionProvider,
                               PlatformTransactionManager transactionManager) {
    JaversSqlRepository sqlRepository = SqlRepositoryBuilder
        .sqlRepository()
        .withConnectionProvider(connectionProvider)
        .withDialect(dialectName)
        .withSchema(preferredSchema)
        .build();

    JaVersDateProvider customDateProvider = new JaVersDateProvider();

    return TransactionalJaversBuilder
        .javers()
        .withTxManager(transactionManager)
        .registerJaversRepository(sqlRepository)
        .withObjectAccessHook(new HibernateUnproxyObjectAccessHook())
        .withListCompareAlgorithm(
            ListCompareAlgorithm.valueOf(javersProperties.getAlgorithm().toUpperCase()))
        .withMappingStyle(
            MappingStyle.valueOf(javersProperties.getMappingStyle().toUpperCase()))
        .withNewObjectsSnapshot(javersProperties.isNewObjectSnapshot())
        .withPrettyPrint(javersProperties.isPrettyPrint())
        .withTypeSafeValues(javersProperties.isTypeSafeValues())
        .withPackagesToScan(javersProperties.getPackagesToScan())
        .withDateTimeProvider(customDateProvider)
        .build();
  }

  /**
   * Configures the Flyway migration strategy to clean the DB before migration first. This is used
   * as the default unless the Spring Profile "production" is active.
   *
   * @return the clean-migrate strategy
   */
  @Bean
  @Profile("!production")
  public FlywayMigrationStrategy cleanMigrationStrategy() {
    return flyway -> {
      logger.info("Using clean-migrate flyway strategy -- production profile not active");
      flyway.clean();
      flyway.migrate();
    };
  }

  @Bean
  public Clock clock() {
    return Clock.system(ZoneId.of(timeZoneId));
  }

  /**
   * Clusters database tables to improve performance. This is run periodically based on the cron 
   * expression.
   */
  @Scheduled(cron = "${db.clustering.cron.expression}")
  public void clusterDatabase() {

    if (dbClusteringEnabled) {
      logger.info("Clustering requisition_line_items");
      template.execute("CLUSTER requisition.requisition_line_items"
          + " USING requisition_line_items_requisitionid_idx;");
      logger.info("Finished clustering requisition_line_items");
    }
  }
}
