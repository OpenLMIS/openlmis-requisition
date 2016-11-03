package org.openlmis.requisition.domain;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Arrays;
import java.util.Collection;

@RunWith(Parameterized.class)
public class RequisitionStatusTest {

  private RequisitionStatus status;
  private boolean preAuthorize;
  private boolean postSubmitted;

  /**
   * Creates a new instance of RequisitionStatusTest. It is used by JUnit to create a parameterized
   * test. It should not be used manually.
   *
   * @param status        requisition status
   * @param preAuthorize  flag that is used to check if the given status should be preAuthorize
   * @param postSubmitted flag that is used to check if the given status should be postSubmitted
   */
  public RequisitionStatusTest(RequisitionStatus status, boolean preAuthorize,
                               boolean postSubmitted) {
    this.status = status;
    this.preAuthorize = preAuthorize;
    this.postSubmitted = postSubmitted;
  }

  /**
   * Creates data needed to initialize each test.
   *
   * @return a collections of arrays that contain data needed to create a new instance of the test.
   */
  @Parameterized.Parameters(name = "status = {0}, preAuthorize = {1}, postSubmitted = {2}")
  public static Collection<Object[]> data() {
    return Arrays.asList(new Object[][]{
        {RequisitionStatus.INITIATED, true, false},
        {RequisitionStatus.SUBMITTED, true, true},
        {RequisitionStatus.AUTHORIZED, false, true},
        {RequisitionStatus.APPROVED, false, true},
        {RequisitionStatus.RELEASED, false, true},
        {RequisitionStatus.SKIPPED, false, false},
    });
  }

  @Test
  public void shouldHaveCorrectValueForPreAuthorize() throws Exception {
    assertThat(status.isPreAuthorize(), is(equalTo(preAuthorize)));
  }

  @Test
  public void shouldHaveCorrectValueForPostSubmitted() throws Exception {
    assertThat(status.isPostSubmitted(), is(equalTo(postSubmitted)));
  }

}
