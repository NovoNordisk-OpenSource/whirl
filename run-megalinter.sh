#!/bin/bash
# Ensure Docker is accessible
export PATH="/opt/homebrew/bin:$PATH"

# Check if Docker is running
if ! docker info >/dev/null 2>&1; then
  echo "ERROR: Docker is not running. Please start Docker Desktop."
  exit 1
fi

# Print diagnostic info
echo "Using Docker at: $(which docker)"
echo "Docker version: $(docker --version)"

# Pull the MegaLinter image if needed
echo "Pulling MegaLinter image..."
docker pull oxsecurity/megalinter:v8.7.0

# Set default parameters
DEFAULT_WORKSPACE="$(pwd)"
DEFAULT_CONFIG_FILE="${DEFAULT_WORKSPACE}/.mega-linter.yml"

# Run MegaLinter directly with Docker
echo "Running MegaLinter..."
docker run --rm \
  -v "${DEFAULT_WORKSPACE}:/tmp/lint" \
  -e "MEGALINTER_CONFIG=/tmp/lint/.mega-linter.yml" \
  -e "DEFAULT_WORKSPACE=/tmp/lint" \
  -e "FLAVOR=r" \
  oxsecurity/megalinter:v8.7.0 "$@"

exit_code=$?
echo "MegaLinter completed with exit code: ${exit_code}"
exit ${exit_code}