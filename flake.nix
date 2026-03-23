# WARN:
# DO NOT USE THIS FLAKE !
# Yeah this is irreproducible and specific to my machine. Deal with it !
{
    description = "Note taking app";

    inputs = {
        programming-flakes.url = "path:/home/chris/Flakes/Programming";
    };

    outputs = { programming-flakes,... }: let
        inherit (programming-flakes.my_util) forEachSystem pkgsFor;
    in {
        devShells = forEachSystem (system: {
            default = programming-flakes.haskell.standard.devShells.${system}.default.overrideAttrs ( o: {
                nativeBuildInputs = o.nativeBuildInputs ++ (with pkgsFor.${system}; [
                    pkg-config
                ]);
            });
        });
    };
}
