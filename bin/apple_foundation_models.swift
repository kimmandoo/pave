import Foundation
import FoundationModels

private let maxInputBytes = 4 * 1024 * 1024
private let maxResponseBytes = 4 * 1024 * 1024
private let textEventBytes = 64 * 1024

private struct Input: Decodable {
    let action: String?
    let model: String?
    let messages: [Message]?
}

private struct Message: Decodable {
    let role: String
    let content: String?
    let toolCalls: [ToolCall]?
    let toolCallId: String?

    enum CodingKeys: String, CodingKey {
        case role, content
        case toolCalls = "tool_calls"
        case toolCallId = "tool_call_id"
    }
}

private struct ToolCall: Decodable {
    let id: String?
    let function: Function
}

private struct Function: Decodable {
    let name: String
    let arguments: String
}

private struct HelperFailure: LocalizedError {
    let message: String
    var errorDescription: String? { message }
}

private func send(_ event: [String: String]) throws {
    let data = try JSONSerialization.data(withJSONObject: event, options: [.sortedKeys])
    FileHandle.standardOutput.write(data)
    FileHandle.standardOutput.write(Data([0x0a]))
}

private func sendText(_ text: String) throws {
    var chunk = ""
    var chunkBytes = 0
    for scalar in text.unicodeScalars {
        chunk.unicodeScalars.append(scalar)
        chunkBytes += scalar.utf8.count
        if chunkBytes >= textEventBytes {
            try send(["type": "text", "text": chunk])
            chunk.removeAll(keepingCapacity: true)
            chunkBytes = 0
        }
    }
    if !chunk.isEmpty {
        try send(["type": "text", "text": chunk])
    }
}

private func fail(_ message: String) -> Never {
    try? send(["type": "error", "message": message])
    exit(1)
}

@available(macOS 26.0, *)
private func ensureModelAvailable() throws {
    switch SystemLanguageModel.default.availability {
    case .available:
        return
    case .unavailable(.deviceNotEligible):
        throw HelperFailure(message: "Apple Foundation Models requires an Apple Intelligence-eligible Apple-silicon Mac.")
    case .unavailable(.appleIntelligenceNotEnabled):
        throw HelperFailure(message: "Apple Intelligence is disabled; enable it in System Settings before using Apple Foundation Models.")
    case .unavailable(.modelNotReady):
        throw HelperFailure(message: "The on-device Apple Foundation Model is not ready; finish its download or preparation first.")
    case .unavailable(let reason):
        throw HelperFailure(message: "Apple Foundation Models is unavailable: \(reason).")
    @unknown default:
        throw HelperFailure(message: "Apple Foundation Models is unavailable on this Mac.")
    }
}

private func makePrompt(_ messages: [Message]) throws -> (instructions: String, prompt: String) {
    var instructions: [String] = []
    var turns: [String] = []

    for message in messages {
        let content = message.content ?? ""
        switch message.role {
        case "system", "developer":
            if !content.isEmpty { instructions.append(content) }
        case "user":
            turns.append("User:\n\(content)")
        case "assistant":
            if !content.isEmpty { turns.append("Assistant:\n\(content)") }
            for call in message.toolCalls ?? [] {
                turns.append("Prior Pave tool request (already represented in history; do not repeat it): \(call.function.name)(\(call.function.arguments)) [call \(call.id ?? "unknown")]")
            }
        case "tool":
            turns.append("Pave tool result for \(message.toolCallId ?? "unknown call"):\n\(content)")
        default:
            throw HelperFailure(message: "Apple Foundation Models received an unsupported conversation role.")
        }
    }

    guard turns.contains(where: { $0.hasPrefix("User:\n") }) else {
        throw HelperFailure(message: "Apple Foundation Models requires a user message.")
    }

    instructions.append("You are using Apple's OS-managed on-device model through Pave. This route has no Pave workspace tools and cannot inspect or change files, run commands, or access external services. Do not claim those actions occurred. Answer only from the supplied conversation; ask the user to switch to a tool-enabled model when workspace access is required.")
    let prompt = turns.joined(separator: "\n\n") + "\n\nAssistant:\n"
    return (instructions.joined(separator: "\n\n"), prompt)
}

@available(macOS 26.0, *)
private func complete(_ input: Input) async throws {
    guard input.model == "default" else {
        throw HelperFailure(message: "Apple Foundation Models uses the OS-managed model ID 'default'.")
    }
    guard let messages = input.messages, !messages.isEmpty else {
        throw HelperFailure(message: "Apple Foundation Models received no conversation messages.")
    }
    try ensureModelAvailable()
    let (instructions, prompt) = try makePrompt(messages)
    let session = LanguageModelSession(instructions: instructions)
    let stream = session.streamResponse(to: prompt)
    var previous = ""

    for try await partial in stream {
        let current = partial.content
        guard current.utf8.starts(with: previous.utf8) else {
            throw HelperFailure(message: "Apple Foundation Models revised already-streamed text; the response was rejected.")
        }
        let delta = String(decoding: current.utf8.dropFirst(previous.utf8.count), as: UTF8.self)
        if !delta.isEmpty {
            guard current.utf8.count <= maxResponseBytes else {
                throw HelperFailure(message: "Apple Foundation Models response exceeds the 4 MiB limit.")
            }
            try sendText(delta)
        }
        previous = current
    }

    guard !previous.isEmpty else {
        throw HelperFailure(message: "Apple Foundation Models returned an empty response.")
    }
    try send(["type": "done", "message": "complete"])
}

@main
private struct PaveAppleFoundationModels {
    static func main() async {
        if CommandLine.arguments.contains("--help") {
            print("Pave Apple Foundation Models helper; macOS 26+ on-device text completion")
            return
        }
        var data = Data()
        while data.count <= maxInputBytes {
            let remaining = maxInputBytes + 1 - data.count
            let chunk = FileHandle.standardInput.readData(ofLength: min(65_536, remaining))
            if chunk.isEmpty { break }
            data.append(chunk)
        }
        guard !data.isEmpty, data.count <= maxInputBytes else {
            fail("Apple model helper input is empty or exceeds the 4 MiB limit.")
        }
        let input: Input
        do {
            input = try JSONDecoder().decode(Input.self, from: data)
        } catch {
            fail("Apple model helper received invalid request JSON.")
        }

        do {
            switch input.action ?? "complete" {
            case "availability":
                if #available(macOS 26.0, *) {
                    try ensureModelAvailable()
                    try send(["type": "availability", "state": "available"])
                } else {
                    throw HelperFailure(message: "Apple Foundation Models requires macOS 26 or later.")
                }
            case "complete":
                if #available(macOS 26.0, *) {
                    try await complete(input)
                } else {
                    throw HelperFailure(message: "Apple Foundation Models requires macOS 26 or later.")
                }
            default:
                throw HelperFailure(message: "Apple model helper received an unsupported action.")
            }
        } catch let failure as HelperFailure {
            fail(failure.message)
        } catch {
            fail("Apple Foundation Models request failed: \(error.localizedDescription)")
        }
    }
}
