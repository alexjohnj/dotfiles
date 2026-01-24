# Deploying Pibox

Pibox is `aarch64-linux` and headless, so it's built as an SD card image on
Pikachu using its `boot.binfmt.emulatedSystems` QEMU emulation.

## New device

The `hashedPasswordFile` secret is encrypted to a fixed host SSH key
(`system.pibox` in `nix-secrets/files/secrets.nix`). A fresh SD card will
generate its own new host key on first boot, which won't match, breaking
password/console login until fixed. To avoid that chicken-and-egg problem,
generate the new device's key and re-encrypt the secret to it *before* flashing:

1. Generate a new host key for the device:

   ```sh
   ssh-keygen -t ed25519 -N "" -C root@pibox -f pibox_host_key
   ```

2. In `nix-secrets`, replace `system.pibox` in `files/secrets.nix` with the
   contents of `pibox_host_key.pub`, then rekey:

   ```sh
   cd nix-secrets/files
   nix develop
   agenix --rekey
   ```

   Commit and push `nix-secrets`, then update the pin in this repo:

   ```sh
   nix flake update secrets
   ```

3. Build the SD image:

   ```sh
   nix build '.#nixosConfigurations.pibox.config.system.build.sdImage'
   ```

4. Flash `result/sd-image/*.img` to the SD card. Double check the target device
   with `lsblk` first.

   ```sh
   sudo dd if=result/sd-image/*.img of=/dev/sdX bs=4M status=progress conv=fsync
   sync
   ```

5. Before ejecting the card, seed it with the host key from step 1 so the image
   doesn't generate a mismatched one on first boot. Mount the card's root
   partition (not the FAT firmware partition) and copy the key in:

   ```sh
   sudo mount /dev/sdX2 /mnt   # confirm the root partition number with lsblk
   sudo install -m 600 -o root -g root pibox_host_key /mnt/etc/ssh/ssh_host_ed25519_key
   sudo install -m 644 -o root -g root pibox_host_key.pub /mnt/etc/ssh/ssh_host_ed25519_key.pub
   sudo umount /mnt
   ```

6. Insert the card, connect ethernet, and power it on. Find its IP address, then
   SSH in as `alex` (one of the keys baked into `configuration.nix` will already
   work):

   ```sh
   ssh alex@<pibox-ip>
   ```

   Password login and `sudo` should work immediately.

7. Tailscale (no auth key is configured, so this is a manual, one-time step):

   ```sh
   sudo tailscale up
   ```

## Updating an existing device

Once a device is up and reachable, deploy config changes the same way as any
remote NixOS host: build locally on Pikachu (via QEMU emulation) and activate on
Pibox over SSH. Using [`nh`](https://github.com/nix-community/nh):

```sh
nh os switch .#pibox --target-host alex@<pibox-host-or-ip>
```

Or with plain `nixos-rebuild`:

```sh
nixos-rebuild switch \
  --flake .#pibox \
  --target-host alex@<pibox-host-or-ip> \
  --use-remote-sudo
```

Both will prompt for `alex`'s sudo password on the remote side. 
