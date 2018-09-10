// swift-tools-version:4.2

import PackageDescription

let package = Package(
    name: "Flipper",
    products: [
        .library(
            name: "Flipper",
            targets: ["Flipper"]),
    ],
    dependencies: [
    ],
    targets: [
        .systemLibrary(
            name: "Clibflipper",
            path: "languages/swift/Clibflipper",
            pkgConfig: "flipper"),
        .target(
            name: "Flipper",
            dependencies: ["Clibflipper"],
            path: "languages/swift/Flipper"),
        .target(
            name: "Example",
            dependencies: ["Flipper"],
            path: "languages/swift/Example"),
    ]
)
