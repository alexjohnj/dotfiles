function gen_ios_icons -d "Generate icons for an iOS app from a single image in the current directory."
    if [ (count $argv) -ne 1 ]
        printf "USAGE: gen_ios_icons BASE_ICON\n"
        return 1
    end

    set -l BASE_ICON $argv[1]

    # iPhone Icons

    sips -Z 40 --out iPhoneNotification-20x20@2x.png $BASE_ICON
    sips -Z 60 --out iPhoneNotification-20x20@3x.png $BASE_ICON

    sips -Z 58 --out iPhoneSettings-29x29@2x.png $BASE_ICON
    sips -Z 87 --out iPhoneSettings-29x29@3x.png $BASE_ICON

    sips -Z 80 --out iPhoneSpotlight-40x40@2x.png $BASE_ICON
    sips -Z 120 --out iPhoneSpotlight-40x40@3x.png $BASE_ICON

    sips -Z 120 --out iPhoneApp-60x60@2x.png $BASE_ICON
    sips -Z 180 --out iPhoneApp-60x60@3x.png $BASE_ICON

    # iPad icons

    sips -Z 20 --out iPadNotification-20x20.png $BASE_ICON
    sips -Z 40 --out iPadNotification-20x20@2x.png $BASE_ICON

    sips -Z 29 --out iPadSettings-29x29.png $BASE_ICON
    sips -Z 58 --out iPadSettings-29x29@2x.png $BASE_ICON

    sips -Z 40 --out iPadSpotlight-40x40.png $BASE_ICON
    sips -Z 80 --out iPadSpotlight-40x40@2x.png $BASE_ICON

    sips -Z 76 --out iPadApp-76x76.png $BASE_ICON
    sips -Z 152 --out iPadApp-76x76@2x.png $BASE_ICON
    sips -Z 167 --out iPadProApp-73.5x73.5@2x.png $BASE_ICON
end
