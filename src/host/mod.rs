//! Host abstraction for embedding. CLI and browser implement this trait.
//!
//! Engine uses host hooks for I/O and environment. Browser embedding provides
//! a different host than CLI.

use std::cell::RefCell;

thread_local! {
    static CURRENT_HOST: RefCell<Option<*const dyn HostHooks>> = RefCell::new(None);
}

/// Host callbacks. Implement for CLI (stdout) or browser (console, DOM, etc.).
pub trait HostHooks {
    /// Print values to host output. CLI writes to stdout; browser to console.
    fn print(&self, args: &[&str]);
}

/// Run closure with host active. Builtins (e.g. print) use this host.
pub fn with_host<H, R>(host: &H, f: impl FnOnce() -> R) -> R
where
    H: HostHooks + 'static,
{
    CURRENT_HOST.with(|cell| {
        let prev = cell.replace(Some(host as *const dyn HostHooks));
        let result = f();
        cell.replace(prev);
        result
    })
}

/// Called by builtin print. Returns true if host handled it, false for default stdout.
pub(crate) fn print_via_host(args: &[&str]) -> bool {
    CURRENT_HOST.with(|cell| {
        if let Some(ptr) = *cell.borrow() {
            let h = unsafe {
                // SAFETY: ptr is only set inside with_host; we are in that call tree.
                &*ptr
            };
            h.print(args);
            true
        } else {
            false
        }
    })
}

/// CLI host: print to stdout.
pub struct CliHost;

impl HostHooks for CliHost {
    fn print(&self, args: &[&str]) {
        use std::io::Write;
        let mut out = std::io::stdout();
        for (i, s) in args.iter().enumerate() {
            if i > 0 {
                let _ = write!(out, " ");
            }
            let _ = write!(out, "{}", s);
        }
        let _ = writeln!(out);
        let _ = out.flush();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;
    use std::rc::Rc;

    struct TestHost(Rc<RefCell<Vec<String>>>);

    impl HostHooks for TestHost {
        fn print(&self, args: &[&str]) {
            for s in args {
                self.0.borrow_mut().push((*s).to_string());
            }
        }
    }

    #[test]
    fn with_host_custom_print() {
        let captured = Rc::new(RefCell::new(Vec::new()));
        let host = TestHost(captured.clone());
        with_host(&host, || {
            print_via_host(&["hello", "42"]);
        });
        let v = captured.borrow();
        assert_eq!(v.as_slice(), &["hello", "42"]);
    }
}
