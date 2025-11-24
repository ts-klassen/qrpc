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
- **Role:** `roles/cloudflare_tunnel/` installs Cloudflare's `cloudflared`
  connector on a host, creates/updates one or more tunnels via the Cloudflare
  API, writes each tunnel's credentials/config, and manages dedicated systemd
  units that keep them online.
- **Role:** `roles/webarena_indigo/` talks to the Indigo API from the control
  node to create a WebArena Indigo VM, start it, and print the assigned IP
  information.
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
- The role talks to S3 via the modern `boto3+s3://<bucket>/<prefix>` backend.
  Point it at a non-AWS or mock endpoint by setting
  `qrpc_backup_s3_endpoint` (for example `https://s3.internal.example`). When
  present, the backup script passes `--s3-endpoint-url=<value>` so duplicity
  reaches that endpoint without changing the target URL format.
- Store AWS credentials, GPG passphrases, or any sensitive overrides under
  Ansible Vault (`qrpc_backup_aws_access_key_id`,
  `qrpc_backup_aws_secret_access_key`, `qrpc_backup_gpg_passphrase`). You can
  omit them entirely when the host uses an IAM role.
- All helper assets stay under `/opt/qrpc`: the env file lives at
  `/opt/qrpc/etc/qrpc-backup/backup.env`, the backup runner script at
  `/opt/qrpc/bin/qrpc-backup`, the manual restore helper at
  `/opt/qrpc/bin/qrpc-restore`, and duplicity metadata under
  `/opt/qrpc/var/lib/qrpc-backup` (with temp/lock directories alongside). Only
  the systemd unit/timer are placed in `/etc/systemd/system`.
- Run `/opt/qrpc/bin/qrpc-restore --help` for usage details. It supports listing
  the most recent files, showing collection status, or restoring all/single
  paths into a destination directory with optional `--time` and `--force`
  arguments.
- Some S3-compatible endpoints reject ranged downloads, conditional requests,
  or send incorrect flexible checksums when downloading. The role defaults
  `qrpc_backup_disable_s3_if_match: true`, installs a small
  `sitecustomize.py` shim, and runs duplicity with `QRPC_DISABLE_S3_IFMATCH=1`
  so boto3 skips `If-Match`, avoids multipart downloads, and disables checksum
  enforcement. Set the flag to `false` if you need the stricter AWS-style
  behavior.
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

Cloudflare Tunnels
------------------

- `roles/cloudflare_tunnel/` installs the official Cloudflare apt repo (optional), ensures the `cloudflared` package is present, writes your tunnel credentials/config under `/etc/cloudflared`, and registers a dedicated systemd service that runs `cloudflared tunnel run <name>`.
- Provide `cloudflare_api_token` with the Cloudflare Tunnel edit scope, `cloudflare_tunnel_account_id`, and define `cloudflare_tunnels` in each host's vars. Every entry needs a `name` and at least one `ingress` rule; everything else—metrics endpoint, fallback service, warp-routing, origin request knobs, config/service paths, etc.—is overridable per entry.
- Example host-vars snippet:

  ```yaml
  cloudflare_tunnels:
    - name: qrpc-app01
      ingress:
        - hostname: app01.example.net
          service: http://127.0.0.1:8000
  ```
- On every run the role talks to the Cloudflare API: it ensures the tunnel exists, generates or reuses the secret, renders the credentials JSON locally, templates `/etc/cloudflared/*.yml`, and keeps `cloudflared-<name>.service` enabled.
- Deploy it per host with:
  `/opt/qrpc/pkg/bin/ansible-playbook -i ansible/env/real/inventory.yml ansible/playbooks/cloudflare_tunnel.yml -e cloudflare_tunnel_target_host=<fqdn>`
- Optional knobs cover metrics endpoints, fallback ingress rules, warp-routing, extra CLI flags, or custom config/credential paths. Check `ansible/env/example/host_vars/qrpc-app01.example.com.yml` for a starter snippet.

WebArena Indigo
---------------

- `roles/webarena_indigo/` calls the Indigo API from `localhost` to create a WebArena Indigo instance, start it, and report its IP info. Provide the API client ID/secret along with `ssh_key_id`, `region_id`, `os_id`, `plan_id`, and `instance_name` variables (vault them in your real env).
- Run it from the repo root with your private inventory/vars in place:
  `/opt/qrpc/pkg/bin/ansible-playbook -i ansible/env/real/inventory.yml ansible/playbooks/webarena_indigo.yml -e webarena_indigo_instance_name=<name>`
  Always pass the instance name via `-e webarena_indigo_instance_name=...`; the role will fail fast if it is missing.
