This branch of openlmis-requisition is intended as a proof-of-concept for how a Spring Data REST service might be documented and tested. It uses [Spring Rest Docs](http://projects.sping.io/spring-restdocs) along with some minor, custom, utility-code.

Follow these steps to use the project:

1) Run “gradle asciidoctor” in order to run our tests and build our documentation.

2) Navigate to <projectRoot>/build/asciidoc/html5/html5/index.html in order to observe the generated documentation.

Noteworthy parts of the project include:

/src/main/asciidoc/index.adoc – An [AsciiDoc](http://asciidoctor.org/docs/asciidoc-writers-guide/) template that gets turned into the core piece of documentation (the index.html file mentioned above). This file is handwritten, though optionally incorporates the content of files which are auto-generated and placed in /build/generated-snippets.

/src/test/java/org/openlmis/requisition/controller/FacilityRepositoryTest.java – A sample test intended to both exercise and document a RESTful API. Potentially, for any given HTTP-based test which runs and passes, a description of the endpoint’s use is generated and written to /build/generated-snippets.

Complimentary documentation may be found in [Jira](https://openlmis.atlassian.net/browse/OLMIS-671).
