{
  machine1 # ip of first machine
#, machine2 # ip of second machine
#, machine3 # ip of third machine
, sshPort ? 22
, sshUser ? "root"
}:
let
  pkgs = import ../pkgs.nix { };
  # Function that creates description of single iperf server
  makeIPerfMachine = host: {
    host = host; # Host of machine to connect to via SSH
    port = sshPort; # Port of machine to connect to via SSH
    user = sshUser; # User of machine with passwordless sudo and allowed SSH login
    services = { # Set of systemd services to install and start
      iperf = { # key value is used as name of systemd unit file
        unit = pkgs.writeTextFile { # text of the systemd unit
          name = "iperf-unit";
          text = ''
            [Unit]
            Description=Iperf3 bandwidth measurement server
            After=networking.target

            [Service]
            Restart=on-failure
            RestartSec=15
            ExecStart=${pkgs.iperf3}/bin/iperf -s --port 5001
            User=root
            KillMode=process

            [Install]
            WantedBy=multi-user.target
            '';
          };
        enable = true; # auto-start?
      };
    };
    derivations = [pkgs.dante];
    backend = "Ubuntu"; # Which internal backend to use
  };
in {
  # Description of each remote machine
  machines = {
    iperf1 = makeIPerfMachine machine1;
    iperf2 = makeIPerfMachine machine1;
#    iperf3 = makeIPerfMachine machine3;
  };
}
