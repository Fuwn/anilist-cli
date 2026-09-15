#!/usr/bin/env sh

set -eu

image_name="${IMAGE_NAME:-fuwn/anilist}"

case "$*" in
"")
	exec docker build --tag "${image_name}:latest" .
	;;
--push)
	platforms="${PLATFORMS:-linux/amd64,linux/arm64}"
	utc_date_tag="$(date -u +%F)"

	exec docker buildx build \
		--platform "${platforms}" \
		--tag "${image_name}:latest" \
		--tag "${image_name}:${utc_date_tag}" \
		--push \
		.
	;;
*)
	printf '%s\n' 'Usage: scripts/build-docker-image.sh [--push]' >&2
	exit 2
	;;
esac
