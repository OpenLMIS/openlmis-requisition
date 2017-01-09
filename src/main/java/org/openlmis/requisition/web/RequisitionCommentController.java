package org.openlmis.requisition.web;

import org.openlmis.requisition.domain.Comment;
import org.openlmis.requisition.domain.CommentBuilder;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.dto.CommentDto;
import org.openlmis.requisition.exception.CommentNotFoundException;
import org.openlmis.requisition.exception.RequisitionNotFoundException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.RequisitionCommentService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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
  @Autowired
  private RequisitionCommentService commentService;

  @Autowired
  private RequisitionRepository requisitionRepository;

  /**
   * Add comment to the requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/comments", method = RequestMethod.POST)
  public ResponseEntity<?> insertComment(
      @RequestBody CommentDto comment, @PathVariable("id") UUID id)
      throws RequisitionNotFoundException {
    Requisition requisition = requisitionRepository.findOne(comment.getRequisitionId());
    Comment updatedComment = CommentBuilder.newComment(comment, requisition);

    List<Comment> comments = commentService.insertComment(id, updatedComment);
    return new ResponseEntity<>(commentService.exportToDtos(comments), HttpStatus.OK);
  }

  /**
   * s
   * Get all comments for specified requisition.
   */
  @RequestMapping(value = "/requisitions/{id}/comments", method = RequestMethod.GET)
  public ResponseEntity<?> getCommentsForRequisition(
      @PathVariable("id") UUID id) throws RequisitionNotFoundException {
    List<Comment> comments = commentService.findCommentsForRequisition(id);
    return new ResponseEntity<>(commentService.exportToDtos(comments), HttpStatus.OK);
  }

  /**
   * Allows updating comments.
   *
   * @param comment   A comment bound to the request body
   * @param commentId UUID of comment which we want to update
   * @return ResponseEntity containing the updated requisition
   */
  @RequestMapping(value = "/requisitions/comments/{id}", method = RequestMethod.PUT)
  public ResponseEntity<?> updateRequisitionComment(
      @RequestBody CommentDto comment, @PathVariable("id") UUID commentId) {
    Requisition requisition = requisitionRepository.findOne(comment.getRequisitionId());
    Comment updatedComment = CommentBuilder.newComment(comment, requisition);

    Comment result = commentService.updateComment(updatedComment, commentId);
    return new ResponseEntity<>(commentService.exportToDto(result), HttpStatus.OK);
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
      return new ResponseEntity<>(commentService.exportToDto(comment), HttpStatus.OK);
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
    Comment comment = commentService.findComment(commentId);
    if (comment == null) {
      return new ResponseEntity(HttpStatus.NOT_FOUND);
    } else {
      commentService.deleteComment(commentId);
    }
    return new ResponseEntity<Comment>(HttpStatus.NO_CONTENT);
  }
}
