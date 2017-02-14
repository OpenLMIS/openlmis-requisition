package org.openlmis.requisition.service.referencedata;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.refEq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;

import org.junit.Test;
import org.mockito.Mockito;
import org.openlmis.requisition.service.RequestParameters;

import java.util.Collections;
import java.util.UUID;

public class SupervisingUsersReferenceDataServiceTest {

  private UUID supervisoryNode = UUID.randomUUID();
  private UUID right = UUID.randomUUID();
  private UUID program = UUID.randomUUID();

  @Test
  public void testFindAll() {
    SupervisingUsersReferenceDataService spy = spy(new SupervisingUsersReferenceDataService());
    doReturn(Collections.emptyList()).when(spy).findAll(anyString(), any(RequestParameters.class));

    spy.findAll(supervisoryNode, right, program);

    Mockito.verify(spy).findAll(eq(supervisoryNode + "/supervisingUsers"),
        refEq(RequestParameters.init().set("rightId", right).set("programId", program)));
  }
}