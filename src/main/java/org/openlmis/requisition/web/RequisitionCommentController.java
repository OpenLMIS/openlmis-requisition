package org.openlmis.requisition.web;

import org.openlmis.requisition.domain.Comment;
import org.openlmis.requisition.exception.CommentNotFoundException;
import org.openlmis.requisition.exception.RequisitionNotFoundException;
import org.openlmis.requisition.repository.CommentRepository;
import org.openlmis.requisition.service.RequisitionCommentService;
import org.openlmis.utils.ErrorResponse;
import org.openlmis.view.View;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.json.MappingJacksonValue;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseStatus;

import java.util.List;
import java.util.UUID;

/**
 * Controller for adding and retrieving requisition comments.
 */
@Controller
public class RequisitionCommentController extends BaseController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequisitionCommentController.class);

  @Autowired
  private RequisitionCommentService commentService;

  @Autowired
  private CommentRepository commentRepository;

  /**
   * Add comment to the requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/comments", method = RequestMethod.POST)
  public ResponseEntity<MappingJacksonValue> insertComment(@RequestBody Comment comment,
                                                           @PathVariable("id") UUID id)
          throws RequisitionNotFoundException {
    List<Comment> comments = commentService.insertComment(id, comment);
    return commentResponse(comments);
  }

  /**s
   * Get all comments for specified requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/comments", method = RequestMethod.GET)
  public ResponseEntity<MappingJacksonValue> getCommentsForRequisition(
          @PathVariable("id") UUID id) throws RequisitionNotFoundException {
    List<Comment> comments = commentService.findCommentsForRequisition(id);
    return commentResponse(comments);
  }

  /**
   * Allows updating comments.
   *
   * @param comment   A comment bound to the request body
   * @param commentId UUID of comment which we want to update
   * @return ResponseEntity containing the updated requisition
   */
  @RequestMapping(value = "/requisitions/comments/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateRequisitionComment(@RequestBody Comment comment,
                                                    @PathVariable("id") UUID commentId) {
    Comment updatedComment = commentService.updateComment(comment, commentId);
    return commentResponse(updatedComment);
  }

  /**
   * Get chosen comment.
   *
   * @param commentId UUID of comment which we want to get
   * @return Comment.
   */
  @RequestMapping(value = "/requisitions/comments/{id}", method = RequestMethod.GET)
  public ResponseEntity<?> getRequisitionComment(@PathVariable("id") UUID commentId) {
    Comment comment = commentService.findComment(commentId);
    if (comment == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
    } else {
      return commentResponse(comment);
    }
  }

  /**
   * Allows deleting comment.
   *
   * @param commentId UUID of comment which we want to delete
   */
  @RequestMapping(value = "/requisitions/comments/{id}", method = RequestMethod.DELETE)
  @ResponseStatus(HttpStatus.NO_CONTENT)
  public ResponseEntity<?> deleteRequisitionComment(@PathVariable("id") UUID commentId)
          throws CommentNotFoundException {
    Comment comment = commentRepository.findOne(commentId);
    if (comment == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      try {
        commentRepository.delete(comment);
      } catch (DataIntegrityViolationException ex) {
        ErrorResponse errorResponse = new ErrorResponse(
            "An error accurred while deleting requisitionTemplate with id: "
                + commentId, ex.getMessage()
        );
        LOGGER.error(errorResponse.getMessage(), ex);
        return new ResponseEntity(HttpStatus.CONFLICT);
      }
    }
    return new ResponseEntity<Comment>(HttpStatus.NO_CONTENT);
  }

  private ResponseEntity<MappingJacksonValue> commentResponse(Object comments) {
    MappingJacksonValue value = new MappingJacksonValue(comments);
    value.setSerializationView(View.BasicInformation.class);
    return new ResponseEntity<>(value, HttpStatus.OK);
  }
}
