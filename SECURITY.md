# Security Policy

## Supported Versions

The following versions of CAADataBridge are currently supported with security updates:

| Version | Supported          |
| ------- | ------------------ |
| 2.4.x   | :white_check_mark: |
| < 2.4   | :x:                |

## Reporting a Vulnerability

We take security seriously. If you discover a vulnerability in CAADataBridge, please **do not** open a public GitHub issue.

Instead, please report it privately using one of the following methods:

- **GitHub Private Vulnerability Reporting**: Use the [Report a vulnerability](https://github.com/CAA-EBV-CO-OP/CAADataBridge/security/advisories/new) button under the Security tab of this repository.
- **Email**: Send details to the project maintainer directly via your GitHub profile contact.

### What to Include

Please include as much of the following as possible:

- A description of the vulnerability and its potential impact
- Steps to reproduce the issue
- Any relevant version information (R version, OS, package versions)
- Suggested fix or mitigation, if known

### What to Expect

- We will acknowledge receipt of your report within **5 business days**.
- We will provide an assessment and expected timeline for a fix within **10 business days**.
- We will notify you when the vulnerability has been patched and credit you in the release notes (unless you prefer to remain anonymous).

## Scope

CAADataBridge is a local R Shiny application. Key security considerations include:

- **File system access**: The app reads CSV files from the user's local machine.
- **No authentication**: The app is not designed to be exposed to the public internet and has no built-in authentication layer.
- **No external data transmission**: The app does not send user data to any external server.

We recommend running CAADataBridge only in trusted local or internal network environments.
