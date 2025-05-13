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

# Create temporary file for output
TEMP_OUTPUT=$(mktemp)

# Run MegaLinter directly with Docker
echo "Running MegaLinter..."
docker run --rm \
  -v "${DEFAULT_WORKSPACE}:/tmp/lint" \
  -e "MEGALINTER_CONFIG=/tmp/lint/.mega-linter.yml" \
  -e "DEFAULT_WORKSPACE=/tmp/lint" \
  -e "FLAVOR=r" \
  oxsecurity/megalinter:v8.7.0 "$@" > "$TEMP_OUTPUT" 2>&1

exit_code=$?

# Filter output to show only errors
echo "Showing only errors:"
echo "----------------------------------------"
grep -E "ERROR:|error:|❌|\[ERROR\]" "$TEMP_OUTPUT"

# Optionally, if no errors found, show a message
if [ $? -ne 0 ]; then
  echo "No errors found."
fi

# Clean up
rm "$TEMP_OUTPUT"

echo "MegaLinter completed with exit code: ${exit_code}"
exit ${exit_code}