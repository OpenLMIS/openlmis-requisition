package org.openlmis.requisition.repository;

import org.openlmis.requisition.domain.Comment;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface CommentRepository
    extends PagingAndSortingRepository<Comment, UUID> {
}
