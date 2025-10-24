set -e

echo "Building backend..."
spago build

echo "Starting backend..."
spago run
