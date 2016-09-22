package org.openlmis.requisition.service;

import org.openlmis.requisition.domain.Comment;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.requisition.exception.CommentNotFoundException;
import org.openlmis.requisition.exception.RequisitionNotFoundException;
import org.openlmis.requisition.repository.CommentRepository;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.UserReferenceDataService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Service for managing requisition comments.
 * Comments are handled separately from requisition lines.
 */
@Service
public class RequisitionCommentService {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionCommentService.class);

  @Autowired
  private RequisitionRepository requisitionRepository;

  @Autowired
  private CommentRepository commentRepository;

  @Autowired
  private UserReferenceDataService userReferenceDataService;


  /**
   * Inserts a new comment and ties it with a requisition.
   * @param requisitionId the id of the requisition
   * @param comment the comment to be added
   * @return all comments of the requisition
   * @throws RequisitionNotFoundException if the requisition
   *                                      with the given id does not exist
   */
  public List<Comment> insertComment(UUID requisitionId, Comment comment)
          throws RequisitionNotFoundException {
    Requisition requisition = findRequisition(requisitionId);

    String userName =
        (String) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
    Map<String, Object> parameters = new HashMap<>();
    parameters.put("username", userName);
    List<UserDto> users = new ArrayList<>(userReferenceDataService.findAll("search", parameters));
    comment.setAuthorId(users.get(0).getId());
    comment.setRequisition(requisition);

    commentRepository.save(comment);

    return requisition.getComments();
  }

  /**
   * Retrieves all comments for the requisition with the given id.
   * @param requisitionId the id of the requisition
   * @return all comments of the requisition
   * @throws RequisitionNotFoundException if the requisition with the given
   *                                      id does not exist
   */
  public List<Comment> findCommentsForRequisition(UUID requisitionId)
        throws RequisitionNotFoundException {
    Requisition requisition = findRequisition(requisitionId);
    return requisition.getComments();
  }

  /**
   * Updates a comment with the given id or creates a new one if it doesn't exist yet.
   * @param comment the id of the comment
   * @param commentId the id of the comment to udpate
   * @return the newly updated/created comment
   */
  public Comment updateComment(Comment comment, UUID commentId) {
    LOGGER.debug("Updating comment with id: " + commentId);

    Comment requisitionComment = commentRepository.findOne(commentId);

    if (requisitionComment == null) {
      requisitionComment = new Comment();
      requisitionComment.setId(commentId);
    }

    requisitionComment.updateFrom(comment);
    Comment updatedComment = commentRepository.save(requisitionComment);

    LOGGER.debug("Updated comment with id: " + commentId);

    return updatedComment;
  }

  /**
   * Find the comment with the given id.
   * @param commentId the id of the comment
   * @return the comment with the given id or null if it does not exist
   */
  public Comment findComment(UUID commentId) {
    return commentRepository.findOne(commentId);
  }

  /**
   * Deletes a comment with the given id if it exists.
   * @param commentId the id of the comment
   */
  public void deleteComment(UUID commentId) throws CommentNotFoundException {
    Comment comment = commentRepository.findOne(commentId);
    if (comment == null) {
      throw new CommentNotFoundException(commentId);
    }
    commentRepository.delete(comment);
  }

  private Requisition findRequisition(UUID requisitionId) throws RequisitionNotFoundException {
    Requisition requisition = requisitionRepository.findOne(requisitionId);
    if (requisition == null) {
      throw new RequisitionNotFoundException(requisitionId);
    }
    return requisition;
  }
}
