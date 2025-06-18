# Security Policy

## 🔐 Supported Versions

We currently maintain the latest **main branch** of Builder UI.  
Security updates and patches are only applied to this branch.

| Version | Supported |
|---------|-----------|
| main    | ✅ Yes     |
| legacy  | ❌ No      |

---

## 📢 Reporting a Vulnerability

If you discover a **security vulnerability** in Builder UI, please **do not open a public issue**.  
Instead, report it **privately and responsibly**:

### 📬 Contact

- Email: **your-email@example.com**
- Or send a private message via [GitHub Discussions](https://github.com/boscobecker/builder-ui/discussions)

Please include:

- A description of the vulnerability
- Steps to reproduce it (if possible)
- A possible solution or patch (optional but appreciated)

We aim to respond within **72 hours** and will keep your report confidential until a fix is available.

---

## 🔒 Security Best Practices

If you're using Builder UI in production environments:

- Always validate JSON inputs.
- Avoid rendering untrusted JSON data.
- Sanitize user inputs before binding them to UI components.
- Follow Delphi secure coding practices, especially when dealing with file paths, network, or database connections.

---

## 🛠 Disclosure Policy

Once a vulnerability is confirmed and patched:

- A GitHub Security Advisory will be published.
- The changelog will include a note about the fix.
- Credit will be given (with permission) to the reporter.

---

Thank you for helping us keep Builder UI secure! 💙
