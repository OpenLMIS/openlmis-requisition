package org.openlmis.requisition.domain;

public final class CommentBuilder {
  private CommentBuilder() {

  }

  /**
   * Creates new comment object based on data from {@link Comment.Importer}
   *
   * @param importer instance of {@link Comment.Importer}
   * @return new instance of requisition.
   */
  public static Comment newComment(Comment.Importer importer, Requisition requisition) {
    Comment comment = Comment.newComment(importer);
    comment.setRequisition(requisition);
    return comment;
  }
}
