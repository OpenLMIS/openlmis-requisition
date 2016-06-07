package org.openlmis.referencedata.web;

import org.openlmis.referencedata.domain.User;
import org.openlmis.referencedata.repository.UserRepository;
import org.openlmis.referencedata.util.ServiceSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class UserController {

    Logger logger = LoggerFactory.getLogger(UserController.class);

    @Autowired
    private UserRepository userRepository;

    /*
        Partially due to story OLMIS-726, which involves replacing id fileds with non-sequential UUID values,
        this POST endpoint will likely change. It exists in its current form largely as a placeholder.
     */
    @RequestMapping(path = "/api/users/", method = RequestMethod.POST)
    public void createUser(@RequestBody User user)
    {
        if(user == null || user.getId() == null)
        {
            //TODO: Return proper response.

            return;
        }
        else
        {
            userRepository.save(user);

            //TODO: Return proper response.
        }
    }

}