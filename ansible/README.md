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
- **Role:** `roles/qrpc_backup/` configures duplicity-based backups for
  `/opt/qrpc`. It drops a wrapper script, credentials env file, and a
  `qrpc-backup.service`/`.timer` pair so backups run automatically.
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
  This playbook always runs the `qrpc_backup` role, so set
  `qrpc_backup_s3_bucket` and related credentials/endpoint variables before
  deploying or the run will stop at the role's variable assertions.
- `host_vars/<hostname>.yml` – per-host overrides, most importantly
  `qrpc_config_source`. Point it at a file that lives next to your inventory,
  e.g. `qrpc_config_source: "{{ inventory_dir }}/configs/<fqdn>.config"`.
- `configs/*.config` – the real Erlang configs that get copied to the hosts.
- `playbooks/qrpc.yml` – generic wrapper around the `qrpc` role that lives
  alongside the rest of the repository. It requires `-e qrpc_target_host=<fqdn>`
  on every run, ensuring you never deploy without naming the exact host. Add
  more playbooks under `ansible/playbooks/` (database, all-in-one, etc.) as
  needed.

Backups
-------

- Configure the bundled duplicity automation by defining
  `qrpc_backup_s3_bucket` (and optionally `qrpc_backup_s3_prefix`) in your
  private `group_vars/all.yml` before applying the `qrpc_backup` role. The
  default `ansible/playbooks/qrpc.yml` play always includes this role and will
  fail fast if the bucket/target information is missing, ensuring the deployment
  never runs without backups configured.
- Point the role at a non-AWS or mock endpoint by setting
  `qrpc_backup_s3_endpoint` (for example `s3+https://s3.internal.example`). The
  computed target becomes `<endpoint>/<bucket>/<prefix>`, keeping the bucket in
  the path (no `bucket.endpoint` hostnames).
- Store AWS credentials, GPG passphrases, or any sensitive overrides under
  Ansible Vault (`qrpc_backup_aws_access_key_id`,
  `qrpc_backup_aws_secret_access_key`, `qrpc_backup_gpg_passphrase`). You can
  omit them entirely when the host uses an IAM role.
- All helper assets stay under `/opt/qrpc`: the env file lives at
  `/opt/qrpc/etc/qrpc-backup/backup.env`, the runner script at
  `/opt/qrpc/bin/qrpc-backup`, and duplicity metadata under
  `/opt/qrpc/var/lib/qrpc-backup` (with temp/lock directories alongside). Only
  the systemd unit/timer are placed in `/etc/systemd/system`.
- Runtime output is streamed through cronolog into
  `/opt/qrpc/var/log/qrpc-backup/%Y%m/%d-%H.log`, satisfying the repo-wide
  logging convention without relying on journald.
- The role registers
  `qrpc-backup.service`/`qrpc-backup.timer`. The timer defaults to `OnCalendar=daily`
  with a randomized delay; override `qrpc_backup_timer_oncalendar`,
  `qrpc_backup_timer_randomized_delay`, or exclude paths via
  `qrpc_backup_exclude_paths` as needed.
- Duplicity cache/metadata staying in `/opt/qrpc/var/lib/qrpc-backup` means the
  timer will continue incremental runs even if the host reboots. Retention
  defaults to 90 days (`qrpc_backup_retention`) and is enforced after every
  backup run.

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

Cloudflare DNS
--------------

- `roles/cloudflare_dns/` uses the Cloudflare API (community.general collection) to ensure DNS records exist or are removed; it runs entirely on the control node (`hosts: localhost`).
- Define `cloudflare_dns_records` and `cloudflare_api_token` in your private copy of `group_vars/all.yml` (or another vars file) following the example structure in `ansible/env/example/group_vars/all.yml`. Keep that file under Ansible Vault so the token stays encrypted.
- Run it from the repo root whenever you need to sync DNS:
  `/opt/qrpc/pkg/bin/ansible-playbook -i ansible/env/real/inventory.yml ansible/playbooks/cloudflare_dns.yml`
