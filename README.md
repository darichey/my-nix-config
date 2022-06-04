# my-nix-config
My NixOS configuration. Not meant for use by anyone else except as a learning resource.

This configuration was based on Wil T's excellent [NixOS introduction series](https://www.youtube.com/watch?v=QKoQ1gKJY5A&list=PL-saUBvIJzOkjAw_vOac75v-x6EzNzZq-).

## Scripts
These mostly exist just so I don't forget the commands...

Update flake inputs:
```sh
./scripts/update.sh
```

Apply changes to system:
```sh
./scripts/apply.sh
```

## Recurring issues

### /boot out of space
The /boot partition on my desktop is woefully small and sometimes runs out of space when applying the configuration. I have found that this can be fixed by a combination of...

* `sudo rm -r /boot/kernels`
* `sudo nix-env -p /nix/var/nix/profiles/system --delete-generations +2`

Also of note: if something goes wrong, pass `--install-bootloader` to `nixos-rebuild` to reinstall the bootloader.

### Existing xmonad file is in the way
Not sure why this happens, doing `rm /home/david/.xmonad/xmonad-x86_64-linux` fixes it.
