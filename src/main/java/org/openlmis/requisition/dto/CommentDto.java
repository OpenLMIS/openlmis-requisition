package org.openlmis.requisition.dto;

import org.openlmis.requisition.domain.Comment;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class CommentDto implements Comment.Exporter, Comment.Importer {

  private UUID id;
  private UUID authorId;
  private String body;
  private LocalDateTime createdDate;
}
