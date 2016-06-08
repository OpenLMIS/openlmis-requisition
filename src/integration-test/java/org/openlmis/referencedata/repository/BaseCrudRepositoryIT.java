package org.openlmis.referencedata.repository;

import org.junit.After;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openlmis.referencedata.Application;
import org.openlmis.referencedata.domain.BaseEntity;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.data.repository.CrudRepository;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.concurrent.atomic.AtomicInteger;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public abstract class BaseCrudRepositoryIT<T extends BaseEntity> {

    abstract CrudRepository<T, Integer> getRepository();

    abstract T generateInstance();

    private AtomicInteger instanceNumber = new AtomicInteger(0);

    int getNextInstanceNumber() {
        return this.instanceNumber.incrementAndGet();
    }

    protected void assertInstance(T instance) {
        Assert.assertNotNull(instance.getId());
    }

    @After
    public void cleanUp() {
        this.getRepository().deleteAll();
    }

    @Test
    public void testCreate() {
        CrudRepository<T, Integer> repository = this.getRepository();

        T instance = this.generateInstance();
        Assert.assertNull(instance.getId());

        instance = repository.save(instance);
        assertInstance(instance);

        Assert.assertTrue(repository.exists(instance.getId()));
    }

    @Test
    public void testFindOne() {
        CrudRepository<T, Integer> repository = this.getRepository();

        T instance = this.generateInstance();

        instance = repository.save(instance);
        assertInstance(instance);

        Integer id = instance.getId();

        instance = repository.findOne(id);
        assertInstance(instance);
        Assert.assertEquals(id, instance.getId());
    }

    @Test
    public void testDelete() {
        CrudRepository<T, Integer> repository = this.getRepository();

        T instance = this.generateInstance();
        Assert.assertNotNull(instance);

        instance = repository.save(instance);
        assertInstance(instance);

        Integer id = instance.getId();

        repository.delete(id);
        Assert.assertFalse(repository.exists(id));
    }
}
