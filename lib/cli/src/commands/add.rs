use std::process::{Command, Stdio};

use anyhow::{Context, Error};
use clap::Parser;
use wasmer_registry::{Bindings, PartialWapmConfig, ProgrammingLanguage};

/// Add a WAPM package's bindings to your application.
#[derive(Debug, Parser)]
pub struct Add {
    /// The registry to fetch bindings from.
    #[clap(long, env = "WAPM_REGISTRY")]
    registry: Option<String>,
    /// Add the JavaScript bindings using "npm install".
    #[clap(long, groups = &["bindings", "js"])]
    npm: bool,
    /// Add the JavaScript bindings using "yarn add".
    #[clap(long, groups = &["bindings", "js"])]
    yarn: bool,
    /// Add the package as a dev-dependency.
    #[clap(long, requires = "js")]
    dev: bool,
    /// Add the Python bindings using "pip install".
    #[clap(long, groups = &["bindings", "py"])]
    pip: bool,
    /// The packages to add (e.g. "wasmer/wasmer-pack@0.5.0" or "python/python")
    #[clap(parse(try_from_str))]
    packages: Vec<wasmer_registry::Package>,
}

impl Add {
    /// Execute [`Add`].
    pub fn execute(&self) -> Result<(), Error> {
        anyhow::ensure!(!self.packages.is_empty(), "No packages specified");

        let registry = self
            .registry()
            .context("Unable to determine which registry to use")?;

        let bindings = self.lookup_bindings(&registry)?;

        let mut cmd = self.target()?.command(&bindings)?;
        cmd.stdin(Stdio::null())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit());

        println!("Running: {cmd:?}");

        let status = cmd.status().with_context(|| {
            format!(
                "Unable to start \"{:?}\". Is it installed?",
                cmd.get_program()
            )
        })?;

        anyhow::ensure!(status.success(), "Command failed: {:?}", cmd);

        Ok(())
    }

    fn lookup_bindings(&self, registry: &str) -> Result<Vec<Bindings>, Error> {
        println!("Querying WAPM for package bindings");

        let mut bindings_to_add = Vec::new();
        let language = self.target()?.language();

        for pkg in &self.packages {
            let bindings = lookup_bindings_for_package(registry, pkg, &language)
                .with_context(|| format!("Unable to find bindings for {pkg}"))?;
            bindings_to_add.push(bindings);
        }

        Ok(bindings_to_add)
    }

    fn registry(&self) -> Result<String, Error> {
        match &self.registry {
            Some(r) => Ok(r.clone()),
            None => {
                let wasmer_dir =
                    PartialWapmConfig::get_wasmer_dir().map_err(|e| anyhow::anyhow!("{e}"))?;
                let cfg = PartialWapmConfig::from_file(&wasmer_dir)
                    .map_err(Error::msg)
                    .context("Unable to load WAPM's config file")?;
                Ok(cfg.registry.get_current_registry())
            }
        }
    }

    fn target(&self) -> Result<Target, Error> {
        match (self.pip, self.npm, self.yarn) {
            (false, false, false) => Err(anyhow::anyhow!(
                "at least one of --npm, --pip or --yarn has to be specified"
            )),
            (true, false, false) => Ok(Target::Pip),
            (false, true, false) => Ok(Target::Npm { dev: self.dev }),
            (false, false, true) => Ok(Target::Yarn { dev: self.dev }),
            _ => Err(anyhow::anyhow!(
                "only one of --npm, --pip or --yarn has to be specified"
            )),
        }
    }
}

fn lookup_bindings_for_package(
    registry: &str,
    pkg: &wasmer_registry::Package,
    language: &ProgrammingLanguage,
) -> Result<Bindings, Error> {
    let all_bindings =
        wasmer_registry::list_bindings(registry, &pkg.package(), pkg.version.as_deref())?;

    match all_bindings.iter().find(|b| b.language == *language) {
        Some(b) => {
            #[cfg(feature = "debug")]
            {
                let Bindings { url, generator, .. } = b;
                log::debug!("Found {pkg} bindings generated by {generator} at {url}");
            }

            Ok(b.clone())
        }
        None => {
            if all_bindings.is_empty() {
                anyhow::bail!("The package doesn't contain any bindings");
            } else {
                todo!();
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Target {
    Pip,
    Yarn { dev: bool },
    Npm { dev: bool },
}

impl Target {
    fn language(self) -> ProgrammingLanguage {
        match self {
            Target::Pip => ProgrammingLanguage::PYTHON,
            Target::Yarn { .. } | Target::Npm { .. } => ProgrammingLanguage::JAVASCRIPT,
        }
    }

    /// Construct a command which we can run to add packages.
    ///
    /// This deliberately runs the command using the OS shell instead of
    /// invoking the tool directly. That way we can handle when a version
    /// manager (e.g. `nvm` or `asdf`) replaces the tool with a script (e.g.
    /// `npm.cmd` or `yarn.ps1`).
    ///
    /// See <https://github.com/wasmerio/wapm-cli/issues/291> for more.
    fn command(self, packages: &[Bindings]) -> Result<Command, Error> {
        let command_line = match self {
            Target::Pip => {
                if Command::new("pip").arg("--version").output().is_ok() {
                    "pip install"
                } else if Command::new("pip3").arg("--version").output().is_ok() {
                    "pip3 install"
                } else if Command::new("python").arg("--version").output().is_ok() {
                    "python -m pip install"
                } else if Command::new("python3").arg("--version").output().is_ok() {
                    "python3 -m pip install"
                } else {
                    return Err(anyhow::anyhow!(
                        "neither pip, pip3, python or python3 installed"
                    ));
                }
            }
            Target::Yarn { dev } => {
                if Command::new("yarn").arg("--version").output().is_err() {
                    return Err(anyhow::anyhow!("yarn not installed"));
                }
                if dev {
                    "yarn add --dev"
                } else {
                    "yarn add"
                }
            }
            Target::Npm { dev } => {
                if Command::new("npm").arg("--version").output().is_err() {
                    return Err(anyhow::anyhow!("yarn not installed"));
                }
                if dev {
                    "npm install --dev"
                } else {
                    "npm install"
                }
            }
        };
        let mut command_line = command_line.to_string();

        for pkg in packages {
            command_line.push(' ');
            command_line.push_str(&pkg.url);
        }

        if cfg!(windows) {
            let mut cmd = Command::new("cmd");
            cmd.arg("/C").arg(command_line);
            Ok(cmd)
        } else {
            let mut cmd = Command::new("sh");
            cmd.arg("-c").arg(command_line);
            Ok(cmd)
        }
    }
}
