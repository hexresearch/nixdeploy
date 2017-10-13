{
  testMachineHost ? "127.0.0.1"
, testMachinePort ? "22"
, testMachineUser ? "root"
}:
let
  pkgs = import ../pkgs.nix { };
  dante-config = pkgs.writeTextFile {
    name = "danted.conf";
    text = ''
      logoutput: syslog /var/log/dante/danted.log
      internal: eth0 port = 1080
      external: eth0

      socksmethod: username
      user.privileged: root
      user.unprivileged: nobody

      client pass {
          from: 0.0.0.0/0 to: 0.0.0.0/0
          log: error
      }

      socks pass {
          from: 0.0.0.0/0 to: 0.0.0.0/0
          command: connect
          log: error
          socksmethod: username
      }
      '';
    };
  dante-unit = pkgs.writeTextFile {
    name = "dante-unit";
    text = ''
      [Unit]
      Description=Dante socks proxy server
      After=networking.target
      ConditionPathExists=!/var/run/sockd.pid

      [Service]
      Restart=on-failure
      RestartSec=15
      ExecStart=${pkgs.dante}/bin/sockd -f ${dante-config} -N 1
      User=root
      ExecReload=/bin/kill -HUP $MAINPID
      PIDFile=/var/run/sockd.pid
      KillMode=process

      [Install]
      WantedBy=multi-user.target
      '';
    };
in {
  # General deployment options
  deployment.user = "deploy"; # Which user will own /nix and profile
  # Description of each remote machine
  machines = {
    testMachine = { # Single machine to deploy
      host = testMachineHost; # Host of machine to connect to via SSH
      port = testMachinePort; # Port of machine to connect to via SSH
      user = testMachineUser; # User of machine with passwordless sudo and allowed SSH login
      services = { # Set of systemd services to install and start
        dante = { # key value is used as name of systemd unit file
          unit = dante-unit; # text of the systemd unit
          enable = true; # auto-start?
        };
      };
      directories = [ { # Folders to create, doesn't touch existing folders
          path = "/var/log/dante";
          owner = "root"; # default is deployment user
        }
        ];
      derivations = [pkgs.dante]; # Additional derivations to copy to deployment profile
      backend = "Ubuntu"; # Which internal backend to use
    };
  };
}
