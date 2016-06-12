package org.openlmis.referencedata.web;

import org.openlmis.referencedata.domain.User;
import org.openlmis.referencedata.repository.UserRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.rest.webmvc.RepositoryRestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

/* See:
        https://jira.spring.io/si/jira.issueviews:issue-html/DATAREST-522/DATAREST-522.html
        http://docs.spring.io/spring-data/rest/docs/2.4.0.M1/reference/html/#customizing-sdr.overriding-sdr-response-handlers
 */
@RepositoryRestController
public class UserController
{
    Logger logger = LoggerFactory.getLogger(UserController.class);

    @Autowired
    private UserRepository userRepository;


    //Support the creation of a new user. Note that any optionally-supplied id value will be ignored, and an alternate one generated.
    @RequestMapping(value = "/users", method = RequestMethod.POST)
    public ResponseEntity<?> createUser(@RequestBody User user)
    {
        if(user == null)
        {
            return new ResponseEntity(HttpStatus.BAD_REQUEST);
        }
        else
        {
            User newUser = userRepository.save(user);
            return new ResponseEntity(newUser, HttpStatus.CREATED);
        }
    }
}