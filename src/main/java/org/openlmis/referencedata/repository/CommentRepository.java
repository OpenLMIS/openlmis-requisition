package org.openlmis.referencedata.repository;

import org.openlmis.referencedata.domain.Comment;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.UUID;

public interface CommentRepository
    extends PagingAndSortingRepository<Comment, UUID> {
}
