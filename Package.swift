// swift-tools-version:4.1

import PackageDescription

let package = Package(
    name: "Flipper",
    products: [
        .library(
            name: "Flipper",
            targets: ["Flipper"]),
    ],
    dependencies: [
      .package(url: "../Clibflipper", .branch("master"))
    ],
    targets: [
        .target(
            name: "Flipper",
            dependencies: [],
            path: "languages/swift/Flipper"),
    ]
)
