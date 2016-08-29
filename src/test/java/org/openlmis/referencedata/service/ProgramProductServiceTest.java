package org.openlmis.referencedata.service;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.referencedata.domain.ProgramProduct;
import org.openlmis.referencedata.repository.ProgramProductRepository;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class ProgramProductServiceTest {

  @Mock
  private ProgramProductRepository programProductRepository;

  @InjectMocks
  private ProgramProductService programProductService;

  @Test
  public void shouldFindProgramProductIfMatchedProgramAndFullSupply() {
    Program program = mock(Program.class);
    ProgramProduct programProduct = mock(ProgramProduct.class);
    Boolean isFullSupply = true;

    when(programProductRepository
            .searchProgramProducts(program, isFullSupply))
            .thenReturn(Arrays.asList(programProduct));

    List<ProgramProduct> receivedProgramProducts = programProductService.searchProgramProducts(
            program, isFullSupply);

    assertEquals(1, receivedProgramProducts.size());
    assertEquals(programProduct, receivedProgramProducts.get(0));
  }
}
