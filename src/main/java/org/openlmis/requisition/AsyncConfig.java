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

import java.util.concurrent.Executor;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.i18n.LocaleContext;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

@Configuration
public class AsyncConfig implements AsyncConfigurer {
  private final ThreadPoolTaskExecutor taskExecutor;

  public AsyncConfig() {
    this.taskExecutor = taskExecutor();
  }

  /**
   * Creates a custom async executor that propagates locale from the calling thread
   * to async execution threads. This ensures notifications are sent in
   * the correct language when requisitions are approved/converted.
   */
  public ThreadPoolTaskExecutor taskExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor() {
      @Override
      public void execute(Runnable task) {
        final LocaleContext context = LocaleContextHolder.getLocaleContext();
        super.execute(() -> {
          LocaleContext previousContext = LocaleContextHolder.getLocaleContext();
          LocaleContextHolder.setLocaleContext(context);
          try {
            task.run();
          } finally {
            LocaleContextHolder.setLocaleContext(previousContext);
          }
        });
      }
    };
    executor.initialize();
    return executor;
  }

  @Override
  public Executor getAsyncExecutor() {
    return taskExecutor;
  }
}
