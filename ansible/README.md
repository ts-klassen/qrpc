qrpc Ansible deployment
=======================

This directory ships a single Ansible role plus a fully worked example
environment. Copy the example, tweak it in a gitignored location, and run the
same playbook for every environment by explicitly targeting a host via the
`qrpc_target_host` variable (you can also apply `--limit` for extra safety).

Overview
--------

- **Role:** `roles/qrpc/` installs qrpc on Ubuntu 24.04 hosts. It installs base
  packages, uploads or downloads the release tarball built by `./Build package`,
  runs `install.sh`, copies your Erlang config to `/opt/qrpc/etc/qrpc/qrpc.config`,
  and restarts the service.
- **Examples:** `env/example/` contains an inventory, vars, host-specific config
  files, and `playbook.yml`. Everything under `env/real/` is ignored by git so
  you can mirror the structure privately.

Quick start
-----------

1. Build the release tarball: `./Build package`
2. Bootstrap a private environment (one-time):  
   `cp -R ansible/env/example ansible/env/real`
3. Edit the files in `ansible/env/real/` to match your hosts.
4. Deploy from the repo root (replace the hostname with one from your inventory):

   `/opt/qrpc/pkg/bin/ansible-playbook -i ansible/env/real/inventory.yml ansible/playbooks/qrpc.yml -e qrpc_target_host=qrpc-app01.example.com`

   Passing the FQDN through `qrpc_target_host` is mandatory; add
   `--limit qrpc-app01.example.com` if you want Ansible to double-check. Every
   run replays the packaged installer and refreshes your config.

Files you should customize
--------------------------

- `inventory.yml` – actual hostnames with their SSH settings. You can keep it
  flat (one FQDN per entry, as in the example) or add groups if you truly need
  them; the playbook only cares about host-level definitions.
- `group_vars/all.yml` – shared release settings such as `qrpc_version`,
  source type, and the path to your tarball. The role exposes `qrpc_repo_root`,
  so the default `qrpc_source_path` already points at
  `_release_packages/qrpc-<version>-<arch>.tar.gz` relative to the repo root.
- `host_vars/<hostname>.yml` – per-host overrides, most importantly
  `qrpc_config_source`. Point it at a file that lives next to your inventory,
  e.g. `qrpc_config_source: "{{ inventory_dir }}/configs/<fqdn>.config"`.
- `configs/*.config` – the real Erlang configs that get copied to the hosts.
- `playbooks/qrpc.yml` – generic wrapper around the `qrpc` role that lives
  alongside the rest of the repository. It requires `-e qrpc_target_host=<fqdn>`
  on every run, ensuring you never deploy without naming the exact host. Add
  more playbooks under `ansible/playbooks/` (database, all-in-one, etc.) as
  needed.

Working with real data
----------------------

- Only copy the example into `ansible/env/real/`; never edit the sample files in
  place.
- `ansible/env/real/` is already listed in `.gitignore`, so inventories, SSH
  details, and configs stay local by default.
- You can introduce groups if it helps organize variables, but deployments
  always name a specific host via `qrpc_target_host`, so groups are optional.
- The role never templates your Erlang config: whatever file you point
  `qrpc_config_source` at is copied byte-for-byte to the remote host.
