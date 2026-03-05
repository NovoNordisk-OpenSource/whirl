# Create biocompute logs

BioCompute is a standard for logs of programs for for Bioinformatics
Computational Analyses.

The BioCompute object is a `json` log that can be created based on the
output of
[`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md).

## Usage

``` r
write_biocompute(queue = run("_whirl.yml"), path = "bco.json", ...)
```

## Arguments

- queue:

  Result from
  [`run()`](https://novonordisk-opensource.github.io/whirl/reference/run.md).

- path:

  A character string specifying the file path to write BioCompute log
  to.

- ...:

  Additional arguments parsed to
  [`jsonlite::write_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html).
  Note always uses `auto_unbox = TRUE`.

## Value

(`invisible`) `list` of the biocompute domains and their content.

## Details

The object consists of the following domains:

- **Specifications**:

  - *spec_version*: Version of BioCompute used
    (\`https://w3id.org/biocompute/1.3.0/“)

  - *object_id*: Unique project id

  - *type*: Your project type

  - *etag*: Your `etag` id from the BioCompute Object Portal

- [Provenance
  Domain](https://wiki.biocomputeobject.org/index.php?title=Provenance-domain)

  - This is used to track the history of the BCO. Review and signatures
    go here.

- [Usability
  Domain](https://wiki.biocomputeobject.org/index.php?title=Usability-domain)

  - This is used to improve searchability by allowing a free-text
    description of the BCO.

  - Provide external document.

- [Extension
  Domain](https://wiki.biocomputeobject.org/index.php?title=Extension-domain)

  - This is used to add any additional structured information that is
    not directly covered by the BCO.

- [Description
  Domain](https://wiki.biocomputeobject.org/index.php?title=Description-domain)

  - Contains a structured field for the description of external
    references, the pipeline steps, and the relationship of I/O objects.

  - Provide external document.

  - **Note**: Use of `keywords` and `External_Reference` entries are not
    yet implemented. To use fill out the entries manually after creating
    the BioCompute object.\`

- [Execution
  Domain](https://wiki.biocomputeobject.org/index.php?title=Execution-domain)

  - Contains fields for the execution of the BCO.

  - **Note**: Use of `external_data_endpoints` not implemented. Fill out
    manually afterwards if needed.

- [Parametric
  Domain](https://wiki.biocomputeobject.org/index.php?title=Parametric-domain)

  - Represents the list of parameters customizing the computational flow
    which can affect the output of the calculations.

- [IO
  Domain](https://wiki.biocomputeobject.org/index.php?title=Iodomain)

  - Represents the list of global input and output files created by the
    computational workflow.

- [Error
  Domain](https://wiki.biocomputeobject.org/index.php?title=Error-domain)

  - Defines the empirical and algorithmic limits and error sources of
    the BCO.

  - **Note**: Use of this domain is not clearly defined. It is therefore
    always left empty in the current implementation. If you want to add
    content do so manually after creating the BCO.

See the [BioCompute Object Portal](https://www.biocomputeobject.org) and
the [BioCompute Objects
Wiki](https://wiki.biocomputeobject.org/Main_Page) for more information.
