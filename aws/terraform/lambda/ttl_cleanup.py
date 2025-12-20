import datetime
import os

import boto3


def _parse_iso8601(value: str):
    if not value:
        return None
    try:
        if value.endswith("Z"):
            value = value[:-1] + "+00:00"
        parsed = datetime.datetime.fromisoformat(value)
        if parsed.tzinfo is None:
            parsed = parsed.replace(tzinfo=datetime.timezone.utc)
        return parsed
    except ValueError:
        return None


def handler(event, context):
    project_tag = os.environ.get("PROJECT_TAG", "")
    project_tag_key = os.environ.get("PROJECT_TAG_KEY", "Project")
    expires_tag = os.environ.get("EXPIRES_AT_TAG", "ExpiresAt")

    if not project_tag:
        return {"terminated": [], "error": "PROJECT_TAG not set"}

    ec2 = boto3.client("ec2")
    now = datetime.datetime.now(datetime.timezone.utc)

    filters = [
        {"Name": "instance-state-name", "Values": ["pending", "running", "stopping", "stopped"]},
        {"Name": f"tag:{project_tag_key}", "Values": [project_tag]},
        {"Name": f"tag:{expires_tag}", "Values": ["*"]},
    ]

    expired_instance_ids = []
    paginator = ec2.get_paginator("describe_instances")
    for page in paginator.paginate(Filters=filters):
        for reservation in page.get("Reservations", []):
            for instance in reservation.get("Instances", []):
                tags = {tag["Key"]: tag["Value"] for tag in instance.get("Tags", [])}
                expires_at = _parse_iso8601(tags.get(expires_tag, ""))
                if expires_at and expires_at <= now:
                    expired_instance_ids.append(instance["InstanceId"])

    for chunk_start in range(0, len(expired_instance_ids), 1000):
        chunk = expired_instance_ids[chunk_start:chunk_start + 1000]
        ec2.terminate_instances(InstanceIds=chunk)

    return {"terminated": expired_instance_ids}
