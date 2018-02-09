// swift-tools-version:4.0

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
        .target(
            name: "Flipper",
            dependencies: ["CFlipper"],
            path: "languages/swift/Flipper"),
        .target(
            name: "CFlipper",
            dependencies: [],
            path: "languages/swift/CFlipper"),
    ]
)
