# Changelog

All notable changes to mk-togglers will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Core `mk/toggler` macro with extensive customization options
- `mk/buffer-toggler` for toggling named buffers
- `mk/dired-toggler` for toggling directory browsers
- `mk/file-toggler` for toggling specific files
- `mk/term-toggler` for toggling terminal buffers with remote host support
- Buffer-local memory: togglers remember which buffer opened them
- Comprehensive lifecycle hooks: before/after for hide/show/make/fallback phases
- Display action customization for controlling window behavior
- Remote (TRAMP) path support in terminal togglers
- Automatic SSH connection and directory navigation for remote terminals
- Shell command execution at various lifecycle points for terminal togglers

### Documentation
- Comprehensive README with installation, usage, and examples
- Detailed API reference (API.md)
- Extensive examples file (EXAMPLES.md)
- Contributing guidelines (CONTRIBUTING.md)

## [0.1.0] - YYYY-MM-DD

### Added
- Initial release
- Basic toggler functionality

---

## Template for Future Releases

## [X.Y.Z] - YYYY-MM-DD

### Added
- New features

### Changed
- Changes to existing functionality

### Deprecated
- Features that will be removed in future versions

### Removed
- Features that have been removed

### Fixed
- Bug fixes

### Security
- Security improvements or fixes

---

## Version History Legend

- **Major version (X.0.0)**: Breaking changes, major new features
- **Minor version (0.X.0)**: New features, backward compatible
- **Patch version (0.0.X)**: Bug fixes, backward compatible

## Links

[Unreleased]: https://github.com/cmoxiv/mk-togglers/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/cmoxiv/mk-togglers/releases/tag/v0.1.0
