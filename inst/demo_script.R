out_dir <- "~/atmos/whirl/inst/examples/demo"


# Single file -------------------------------------------------------------------

#Point to a single script
run("~/atmos/whirl/inst/examples/demo/metadata/mdcol.R", summary_dir = out_dir)


# Folders (run in parallel) ----------------------------------------------------

# A single folder
run("~/atmos/whirl/inst/examples/demo/tfl", summary_dir = out_dir)

# Multiple folders
run(c("~/atmos/whirl/inst/examples/demo/metadata",
      "~/atmos/whirl/inst/examples/demo/tfl"), summary_dir = out_dir)


# Config-------------------------------------------------------------------------

# Use a config file
run("~/atmos/whirl/inst/examples/demo/demo_whirl.yaml", summary_dir = out_dir)


# Use a config file (skipping steps after errors)
run("~/atmos/whirl/inst/examples/demo/demo_skip_whirl.yaml", summary_dir = out_dir)

run("~/atmos/whirl/inst/examples/sequence_exuction/test.py")


