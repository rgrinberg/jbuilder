Cram tests are able to see environment variables defined in parent dune files:

  $ echo $CRAM_ENV_TEST_VISIBLE
  visible
  $ echo $CRAM_ENV_TEST_HIDDEN
  invisible
