package org.openlmis.security;

import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.hierarchyandsupervision.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.oauth2.provider.token.DefaultUserAuthenticationConverter;

import java.util.Map;
import java.util.UUID;

public class UserTokenConverter extends DefaultUserAuthenticationConverter {

  @Autowired
  private UserRepository userRepository;

  /**
   * Extracts an Authentication from a map.
   * @param map map containing information about the user.
   * @return authentication token.
     */
  public Authentication extractAuthentication(Map<String, ?> map) {
    UsernamePasswordAuthenticationToken token =
        (UsernamePasswordAuthenticationToken) super.extractAuthentication(map);
    if (token != null) {
      User principal = new User();
      principal.setUsername(token.getPrincipal().toString());
      Object userId = map.get("referenceDataUserId");
      if (userId != null) {
        User user = userRepository.findOne(UUID.fromString((String) userId));
        if (user != null) {
          principal = user;
        }
      }
      return new UsernamePasswordAuthenticationToken(principal, token.getCredentials(),
          token.getAuthorities());
    }
    return null;
  }
}
