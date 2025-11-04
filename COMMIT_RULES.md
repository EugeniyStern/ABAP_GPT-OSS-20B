# Git Commit Rules

## Language Requirement
**All commit messages must be written in English only.**

This is enforced by a git hook that will reject commits with non-English characters.

## Commit Message Format

Follow the conventional commit format:

```
<type>: <subject>

<body>

<footer>
```

### Types
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Maintenance tasks

### Examples

```
feat: add user authentication

Implement login and registration functionality with JWT tokens.
Add password hashing using bcrypt.

Closes #123
```

```
fix: resolve memory leak in data processor

The processor was not releasing resources after processing.
Added proper cleanup in the finally block.

Fixes #456
```

## Setup

The project is configured with:
- `.gitmessage` - Commit message template
- `commit-msg` hook - Enforces English-only messages

The hook runs automatically on every commit.

