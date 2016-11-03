package org.openlmis.utils;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.AuthenticationException;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

@RunWith(MockitoJUnitRunner.class)
public class AuthenticationHelperTest {

  @Mock
  private UserReferenceDataService userReferenceDataService;

  @InjectMocks
  private AuthenticationHelper authenticationHelper;

  @Before
  public void setUp() {
    Authentication authentication = mock(Authentication.class);
    when(authentication.getPrincipal()).thenReturn("username");

    SecurityContext securityContext = mock(SecurityContext.class);
    when(securityContext.getAuthentication()).thenReturn(authentication);

    SecurityContextHolder.setContext(securityContext);
  }

  @Test
  public void shouldReturnUser() {
    // given
    UserDto userMock = mock(UserDto.class);
    when(userReferenceDataService.findUser(any(String.class))).thenReturn(userMock);

    // when
    UserDto user = authenticationHelper.getCurrentUser();

    // then
    assertNotNull(user);
  }

  @Test(expected = AuthenticationException.class)
  public void shouldThrowExceptionIfUserDoesNotExist() {
    // given
    when(userReferenceDataService.findUser(any(String.class))).thenReturn(null);

    // when
    authenticationHelper.getCurrentUser();
  }
}
