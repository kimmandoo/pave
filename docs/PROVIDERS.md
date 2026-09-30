# Pave providers

Run `pave --providers` for the live list. This reference separates wire transport, credentials and model selection; **a listed model is not proof of account access or tool support**. For general usage, see [USAGE.md](USAGE.md).

## Contents

- [Provider routes](#provider-routes)
- [Provider-specific notes](#provider-specific-notes)
- [User-defined OpenAI-compatible providers](#user-defined-openai-compatible-providers)
- [Provider-specific thinking controls](#provider-specific-thinking-controls)
- [Deployment and catalog caveats](#deployment-and-catalog-caveats)

## Provider routes

| Provider | Transport | Authentication | CLI selection |
| --- | --- | --- | --- |
| OpenAI | Chat Completions, Responses | `OPENAI_API_KEY` | `--provider openai --model MODEL_ID` (Responses by default; `--api chat` for Chat-only models) |
| OpenAI Codex subscription | account-scoped Codex Responses | `--login openai-codex` (browser PKCE) or `--login-device openai-codex` (device approval) | `--provider openai-codex --model MODEL_ID` |
| Anthropic | Messages | `ANTHROPIC_API_KEY` or `--login anthropic` | `--provider anthropic --model MODEL_ID` |
| Ollama | native `/api/chat` | none (local server) | `--provider ollama --model MODEL_ID` |
| Google Gemini API | native `generateContent` | `GEMINI_API_KEY` | `--provider google --model MODEL_ID` |
| DeepSeek | Chat Completions | `DEEPSEEK_API_KEY` | `--provider deepseek --model MODEL_ID` |
| Groq | Chat Completions | `GROQ_API_KEY` | `--provider groq --model MODEL_ID` |
| Mistral | Chat Completions | `MISTRAL_API_KEY` | `--provider mistral --model MODEL_ID` |
| OpenRouter | Chat Completions | `OPENROUTER_API_KEY` or `--login openrouter` (PKCE exchanges for API key) | `--provider openrouter --model MODEL_ID` |
| Together AI | Chat Completions | `TOGETHER_API_KEY` | `--provider together --model MODEL_ID` |
| Cerebras | Chat Completions | `CEREBRAS_API_KEY` | `--provider cerebras --model MODEL_ID` |
| Venice | Chat Completions | `VENICE_API_KEY` | `--provider venice --model MODEL_ID` |
| DeepInfra | Chat Completions | `DEEPINFRA_API_KEY` | `--provider deepinfra --model MODEL_ID` |
| [Fireworks AI](https://docs.fireworks.ai/guides/reasoning) | Chat Completions; account models filtered for serverless/tool support; IDs do not prove entitlement | `FIREWORKS_API_KEY` | `--provider fireworks --model MODEL_ID`; `/thinking` sends supported effort and preserves reasoning across tool results |
| Hugging Face Inference | Chat Completions | `HF_TOKEN` | `--provider huggingface --model MODEL_ID` |
| NanoGPT | Chat Completions | `NANO_GPT_API_KEY` | `--provider nanogpt --model MODEL_ID` |
| Azure OpenAI | Azure v1 Responses and Chat | Azure API key or Azure CLI Entra identity | `--provider azure --model DEPLOYMENT_ID` (Responses by default; `--api chat` for Chat) |
| AIML API | Chat Completions | `AIMLAPI_API_KEY` | `--provider aimlapi --model MODEL_ID` |
| ai& | Chat Completions | `AIAND_API_KEY` | `--provider aiand --model MODEL_ID` |
| Sakana AI | Responses | `SAKANA_API_KEY` or `FUGU_API_KEY` | `--provider sakana --model MODEL_ID` |
| Abliteration AI | Responses (default), Chat Completions | `ABLITERATION_API_KEY` or `ABLIT_KEY` | `--provider abliteration --model MODEL_ID` |
| GMI Cloud | Chat Completions | `GMI_API_KEY` | `--provider gmi-cloud --model MODEL_ID` |
| Google Vertex AI | Gemini GenerateContent streaming and Anthropic Messages | Google ADC, gcloud impersonation or explicit access token | `--provider google-vertex --model MODEL_ID` (`--api messages` for Claude) |
| Amazon Bedrock | SigV4 Converse and ConverseStream | AWS credential chain | `--provider amazon-bedrock --model MODEL_ID` |
| Baseten | Chat Completions | `BASETEN_API_KEY` | `--provider baseten --model MODEL_ID` |
| LM Studio (local) | Chat Completions | optional `LM_STUDIO_API_KEY` | `--provider lm-studio --model MODEL_ID` |
| llama.cpp (local) | Chat Completions | optional `LLAMA_CPP_API_KEY` | `--provider llama.cpp --model MODEL_ID` |
| vLLM (local) | Chat Completions | optional `VLLM_API_KEY` | `--provider vllm --model MODEL_ID` |
| Apple Foundation Models (macOS arm64) | on-device Foundation Models text | macOS 26+, Apple silicon, Apple Intelligence; no provider key | `--provider apple --model default` |
| GitHub Copilot (personal github.com) | pinned public Chat Completions endpoint | `--login github-copilot` (device code, `read:user`) | `--provider github-copilot --models` then `--model MODEL_ID` |
| Moonshot AI (global) | Chat Completions | `MOONSHOT_API_KEY` or `KIMI_API_KEY` | `--provider moonshot --model MODEL_ID` |
| Ollama Cloud | native hosted Chat | `OLLAMA_CLOUD_API_KEY` | `--provider ollama-cloud --model MODEL_ID` |
| Bedrock Mantle | regional Responses | `AWS_BEARER_TOKEN_BEDROCK` + AWS region | `--provider bedrock-mantle --model MODEL_ID` |
| xAI | native Chat | `XAI_API_KEY` | `--provider xai --model MODEL_ID` |
| NVIDIA hosted NIM | native Chat | `NVIDIA_API_KEY` | `--provider nvidia --model MODEL_ID` |
| Novita AI | native Chat | `NOVITA_API_KEY` | `--provider novita --model MODEL_ID` |
| SiliconFlow global | native Chat | `SILICONFLOW_API_KEY` | `--provider siliconflow --model MODEL_ID` |
| SiliconFlow China | native regional Chat | `SILICONFLOW_CN_API_KEY` | `--provider siliconflow-cn --model MODEL_ID` |
| StepFun international | native Chat; model IDs unclassified | `STEPFUN_API_KEY` | `--provider stepfun --model KNOWN_CHAT_ID` |
| CoreWeave Serverless (W&B Inference) | native Chat | `COREWEAVE_API_KEY` or `WANDB_API_KEY` | `--provider coreweave --model MODEL_ID` |
| Synthetic | native OpenAI-host Chat; model IDs unclassified | `SYNTHETIC_API_KEY` | `--provider synthetic --model KNOWN_CHAT_ID` |
| Z.AI standard API | native Chat, no documented model listing | `ZAI_API_KEY` | `--provider zai --model KNOWN_CHAT_ID` |
| ZenMux | native Chat; model IDs unclassified | `ZENMUX_API_KEY` | `--provider zenmux --model KNOWN_CHAT_ID` |
| Wafer Serverless | native Chat; model IDs unclassified | `WAFER_SERVERLESS_API_KEY` | `--provider wafer-serverless --model KNOWN_CHAT_ID` |
| Baidu Qianfan V2 | native Chat; only `type=chat` IDs listed | `QIANFAN_API_KEY` | `--provider qianfan --model MODEL_ID` |
| Xiaomi MiMo pay-as-you-go | native Chat; model IDs unclassified | `XIAOMI_API_KEY` | `--provider xiaomi --model KNOWN_CHAT_ID` |
| Kilo | native Chat; public model IDs unclassified | `KILO_API_KEY` or `--login kilo` (device approval) | `--provider kilo --model KNOWN_CHAT_ID` |
| [Alibaba Coding Plan](https://help.aliyun.com/en/model-studio/coding-plan-faq) | Region-pinned subscription Chat; no account model listing | `ALIBABA_CODING_PLAN_API_KEY` (`sk-sp-`) | `--provider alibaba-coding-plan --api china or intl --model KNOWN_MODEL_ID` |
| SingularityAPI universal | native Chat; authenticated model IDs unclassified | `SINGULARITYAPI_DEV_API_KEY` | `--provider singularityapi-dev --model KNOWN_CHAT_ID` |
| SingularityAPI reserved | pinned Chat; live account entitlement unverified | `SINGULARITYAPI_TECH_API_KEY` | `--provider singularityapi-tech --model KNOWN_CHAT_ID` |
| OpenCode Zen / Go | distinct pinned Responses / Chat; public model IDs unclassified | `OPENCODE_API_KEY` | `--provider opencode-zen|opencode-go --model KNOWN_MODEL_ID` |
| Charm Hyper | native Chat; keyless public model IDs unclassified | `CHARM_HYPER_API_KEY` or `HYPER_API_KEY` (`sk-hyper-`) | `--provider charm-hyper --model KNOWN_CHAT_ID` |
| Fire Pass | native Chat; manually supplied full router resource | `FIREPASS_API_KEY` (`fpk_`) | `--provider firepass --model accounts/fireworks/routers/ROUTER_ID` |
| Yolo Auto | native Chat; authenticated model IDs unclassified | `YOLO_AUTO_API_KEY` | `--provider yolo-auto --model KNOWN_CHAT_ID` |
| Xiaomi MiMo Token Plan (AMS / CN / SGP) | three independently pinned subscription Chat regions | `XIAOMI_TOKEN_PLAN_AMS_API_KEY`, `_CN_API_KEY`, `_SGP_API_KEY` respectively (`tp-`/`ttp-`) | `--provider xiaomi-token-plan-ams|xiaomi-token-plan-cn|xiaomi-token-plan-sgp --model KNOWN_CHAT_ID` |
| MiniMax Coding Plan (international / China) | independently pinned subscription Chat regions | `MINIMAX_CODE_API_KEY` / `MINIMAX_CODE_CN_API_KEY` respectively (`sk-cp-`) | `--provider minimax-code|minimax-code-cn --model KNOWN_CHAT_ID` |
| Meta Model API | pinned stateless Responses; authenticated model IDs unclassified | `MODEL_API_KEY` or `META_API_KEY` | `--provider meta --model KNOWN_RESPONSES_ID` |
| Vercel AI Gateway | pinned Chat; listed IDs unclassified | `AI_GATEWAY_API_KEY` or `VERCEL_AI_GATEWAY_API_KEY` | `--provider vercel-ai-gateway --model KNOWN_CHAT_ID` |
| Cloudflare AI Gateway | account/gateway-scoped unified Chat; no authoritative catalog | `CLOUDFLARE_AI_GATEWAY_API_KEY` + `CLOUDFLARE_ACCOUNT_ID` + `CLOUDFLARE_GATEWAY_ID` | `--provider cloudflare-ai-gateway --model PROVIDER/MODEL_OR_DYNAMIC/ROUTE` |
| Command Code Studio Provider API | separate Chat, Messages, Responses; explicit route and model | `COMMAND_CODE_API_KEY` or `COMMANDCODE_API_KEY` (Studio key, not GO-plan credential) | `--provider commandcode --api chat|messages|responses --model KNOWN_ROUTE_MODEL_ID` |
| GitLab Duo Direct Access | account-bound token exchange then Anthropic, Responses or Chat proxy | `GITLAB_TOKEN` PAT or `--login gitlab-duo` (registered `GITLAB_CLIENT_ID` + `GITLAB_REDIRECT_URI`) | `--provider gitlab-duo --api messages|responses|chat --model KNOWN_UPSTREAM_MODEL_ID` |
| Devin CLI | pinned protobuf/Connect Chat and account-scoped models | `DEVIN_API_KEY` session token or `--login devin` (PKCE) | `--provider devin --models`, then `--model ACCOUNT_MODEL_ID` |
| [MiniMax API](https://platform.minimax.io/docs/api-reference/models/openai/list-models) (international) | Chat Completions; `/models` returns unclassified IDs | `MINIMAX_API_KEY` | `--provider minimax --models`, then select a listed ID with `--model MODEL_ID` |
| [Cline Pass](https://github.com/cline/cline/blob/main/docs/api/chat-completions.mdx) | Chat Completions; exact full model ID required, no API listing | `CLINE_API_KEY` | `--provider cline-pass --model cline-pass/MODEL_ID` |
| [Alibaba Token Plan](https://help.aliyun.com/en/model-studio/token-plan-personal-quick-start) (Beijing) | Fixed OpenAI-compatible Chat; explicit model ID | `ALIBABA_TOKEN_PLAN_API_KEY` (`sk-sp-`) | `--provider alibaba-token-plan --model KNOWN_MODEL_ID` |
| [Kimi Code](https://www.kimi.com/code/docs/) (international or China) | Pinned regional Chat or Messages; explicit plan model ID | `KIMI_API_KEY` (Kimi Code key for selected region, not Moonshot API) | `--provider kimi-code` or `kimi-code-cn`, `--api chat` or `messages`, `--model KNOWN_MODEL_ID` |
| [Umans Code](https://app.umans.ai/offers/code/docs) | Chat or Messages (Messages default); `/v1/models/info` reports model capabilities | `UMANS_AI_CODING_PLAN_API_KEY` | `--provider umans --models`, then `--model MODEL_ID` |

## Provider-specific notes

The [provider table](#provider-routes) lists credentials and CLI flags. Additional route caveats:

- **Moonshot / Ollama Cloud:** Moonshot (`MOONSHOT_API_KEY` or `KIMI_API_KEY`) lists models on its global `/v1/models` and uses Chat. Ollama Cloud (`OLLAMA_CLOUD_API_KEY` or `OLLAMA_API_KEY`) uses fixed hosted `/api/tags` and `/api/chat`; the local Ollama route never receives its key.
- **Bedrock Mantle:** `AWS_BEARER_TOKEN_BEDROCK` with `AWS_REGION` selects a validated regional Responses endpoint and bearer-key model listing, separate from SigV4 Bedrock Converse and ConverseStream. A listed model is not guaranteed to support Responses.
- **xAI / NVIDIA:** `XAI_API_KEY` lists account models at `/v1/language-models`; native Chat preserves reported `reasoning_content` on tool replay. `NVIDIA_API_KEY` lists `/v1/models`, but listings omit per-model tool support and hosted schemas vary; Pave does not infer compatibility from names. Both buffer validated completions before displaying text, not incremental SSE.
- **Novita / SiliconFlow:** Novita (`NOVITA_API_KEY`) uses a fixed hosted Chat endpoint, dynamic `/models` and unchanged interleaved reasoning replay. SiliconFlow uses separate global `SILICONFLOW_API_KEY` and China `SILICONFLOW_CN_API_KEY` hosts, text/chat-filtered listings and native Chat; credentials never cross regions. Some models need a vendor-specific thinking switch for tools: no default switch or model-name heuristic is bundled. These routes buffer validated completions.
- **StepFun:** `STEPFUN_API_KEY` is sent only to `api.stepfun.ai`, not the separate `.com` platform. Its `/v1/models` mixes Chat and audio without capability flags; `--models` calls IDs unclassified and `/model` marks them `[listed · API unverified]`. Select only a known compatible ID. Complete StepFun tool calls may end with its documented `finish_reason: "stop"`; other Chat routes retain strict finish validation.
- **Apple Foundation Models:** on macOS 26+ Apple silicon with Apple Intelligence enabled, `apple` uses the OS-managed `default` ID. It is text-only, has no model listing or Pave tools, and is advertised only when its native helper is installed or packaged.
- **Cursor / agent-runtime boundaries:** Cursor's official SDK bridge exposes a model-listing RPC and separate agent-runtime APIs, not a raw completion route; Pave does not present it as generic Chat. GitLab Duo Agent is likewise distinct from the implemented GitLab Duo direct inference APIs and remains unsupported.
- **Copilot:** the supported route is personal `github.com` Chat only. No documented personal Responses/Anthropic route or GitHub Enterprise route is claimed.
- **CoreWeave Serverless:** W&B Inference's fixed host accepts `COREWEAVE_API_KEY` or `WANDB_API_KEY` for native Chat and account `/v1/models`. A CoreWeave control-plane credential is not interchangeable.
- **Alibaba Coding Plan / Token Plan:** `ALIBABA_CODING_PLAN_API_KEY` is the Coding Plan subscription key; select `--api china` or `--api intl` for the matching pinned host and an explicitly supported model. `ALIBABA_TOKEN_PLAN_API_KEY` is separate and reaches only its pinned Beijing Token Plan route. Both plans lack a Pave account-model listing; both may use the `sk-sp-` prefix, but their keys and hosts are not interchangeable. See [Coding Plan](https://help.aliyun.com/en/model-studio/coding-plan-faq) and [Token Plan](https://help.aliyun.com/en/model-studio/token-plan-personal-quick-start).
- **Xiaomi MiMo Token Plan:** obtain the subscription key from the Token Plan console and use the matching provider/region: `xiaomi-token-plan-ams` with `XIAOMI_TOKEN_PLAN_AMS_API_KEY`, `xiaomi-token-plan-cn` with `XIAOMI_TOKEN_PLAN_CN_API_KEY`, or `xiaomi-token-plan-sgp` with `XIAOMI_TOKEN_PLAN_SGP_API_KEY`. These `tp-`/`ttp-` keys are not the pay-as-you-go `XIAOMI_API_KEY`; the Token Plan has no documented model-list endpoint, so select a known supported ID. [Official quick access](https://mimo.mi.com/docs/en-US/tokenplan/Token%20Plan/quick-access).
- **Cloudflare AI Gateway:** configure `CLOUDFLARE_ACCOUNT_ID`, `CLOUDFLARE_GATEWAY_ID` and `CLOUDFLARE_AI_GATEWAY_API_KEY` for that gateway. Configure upstream BYOK credentials or Unified Billing in Cloudflare separately; Pave sends the gateway token only as `cf-aig-authorization`, never as an upstream provider key. Choose a provider-qualified model ID or configured `dynamic/ROUTE`; Pave has no authoritative gateway catalog. [Cloudflare API guide](https://developers.cloudflare.com/ai-gateway/usage/chat-completion/).

These routes passed isolated fake-HTTPS/native workspace tool-result scenarios; no live vendor account entitlement or per-model tool support was verified.

## User-defined OpenAI-compatible providers

Configure custom providers only in `${XDG_CONFIG_HOME:-~/.config}/pave/settings.json`. Project `.pave/settings.json` deliberately rejects `custom_providers`; configured endpoints, route names, account IDs, model IDs, and environment-variable names are not secrets.

```json
{
  "custom_providers": [
    {
      "id": "team-gateway",
      "display_name": "Team Gateway",
      "default_route": "chat",
      "routes": [
        {
          "name": "chat",
          "api": "openai-chat",
          "endpoint": "https://gateway.example/v1/chat/completions",
          "account_id": "team-7",
          "api_key_env": "TEAM_GATEWAY_KEY",
          "models_endpoint": "https://gateway.example/v1/models"
        }
      ]
    }
  ]
}
```

- Only OpenAI Chat Completions is supported. Endpoints must be HTTPS; numeric loopback HTTP (`127.0.0.1` or `[::1]`) is allowed for inference only. The optional model-list endpoint must be HTTPS, same-origin, and end in `/models`.
- `api_key_env` names the environment variable whose value is sent as a Bearer key to that fixed route. Omit it for a keyless route. Never put the key itself in settings.
- Configure exactly one of `models_endpoint` or a static `models` array. Static model entries take an exact `id` and optional `display_name` and explicitly declared `tools` boolean; omitted capability data stays unknown. Neither configuration proves account entitlement.
- Select the default route with `pave --provider team-gateway --model MODEL_ID`; use `--api ROUTE_NAME` for another configured route. A changed route configuration requires reselecting a saved model. Built-in provider counts exclude user-defined providers.

## Provider-specific thinking controls

The TUI `/thinking LEVEL` command stores branch-local metadata; a route sends only a control documented for that provider. Fireworks maps `minimal` to `reasoning_effort: "none"` and replays its returned `reasoning_content` with tool results. Alibaba Coding Plan and Token Plan map `none` to `enable_thinking: false`, other supported levels to `true`, and omit the field by default; model support is not inferred from the model ID. See the linked [Fireworks reasoning](https://docs.fireworks.ai/guides/reasoning) and [Alibaba Qwen Code](https://help.aliyun.com/en/model-studio/qwen-code) guidance.

## Deployment and catalog caveats

- **Copilot:** Interactive `/model` can choose a discovered Chat ID; headless prompts need `--model` or a saved default. The pinned endpoint rejects unauthorized model IDs.

- **Local LM Studio / llama.cpp / vLLM:** Keyless Chat by default; optional `LM_STUDIO_API_KEY`, `LLAMA_CPP_API_KEY` or `VLLM_API_KEY`. Matching `*_BASE_URL` variables allow numeric private/loopback addresses or `localhost` (default ports 1234, 8080, 8000). Listing and chat use the same validated host; discovery ignores `--endpoint`, disables proxies and follows no redirects. A key over plain LAN HTTP is unencrypted: use trusted HTTPS across a network.
- **Azure OpenAI:** Set `AZURE_OPENAI_ENDPOINT=https://<resource>.openai.azure.com` and choose the exact deployment ID. Routes are public-cloud v1 Responses by default or Chat with `--api chat`; authenticate with `AZURE_OPENAI_API_KEY` or the current Azure CLI identity. `--models` discovers deployments through Azure Resource Manager in the current subscription and requires management read access; an API key alone cannot list. Pave does not translate model IDs to deployment names or target sovereign clouds.
- **Google Vertex:** Set `GOOGLE_CLOUD_PROJECT`, `GOOGLE_VERTEX_LOCATION` (or documented aliases) and a known publisher model ID. Google ADC supports authorized-user/service-account credentials directly; impersonated credentials use gcloud, while explicit tokens and metadata credentials remain supported. The registered routes are native Gemini GenerateContent and Vertex Claude Messages (`--api messages`); there is no account model-list API, so IDs remain manual. Cloud Code Assist/Gemini CLI identity is not aliased to Vertex, and Antigravity consumer OAuth is prohibited by its terms.
- **Amazon Bedrock:** Set `AWS_REGION` or `AWS_DEFAULT_REGION` and use the AWS credential chain: environment/shared files, cached SSO, web identity/assume-role, ECS/EC2 metadata; `credential_process` is disabled unless `PAVE_AWS_CREDENTIAL_PROCESS=allow`. SigV4 signs only regional Converse/ConverseStream requests. `--models` lists active on-demand text foundations and inference profiles, not Invoke permission; custom endpoints are rejected. Bedrock Mantle remains a separate regional bearer Responses route.
- **Cursor and GitLab Duo Agent:** Cursor's official [bridge protocol](https://github.com/cursor/sdk-bridge#readme) uses Connect over HTTP/1.1; the [service definitions](https://github.com/cursor/sdk-bridge/blob/main/docs/services.md) separate `SdkCursorService.ListModels` from the agent-run lifecycle. It is not a generic raw-completion provider. GitLab Duo Agent's persistent authenticated WebSocket/runtime is separate from GitLab Duo's supported direct Messages/Responses/Chat routes. No unsupported agent login or route is advertised.
- **Personal Copilot:** only the pinned Chat route and account roster are supported; Responses, Anthropic Messages and GitHub Enterprise require separately documented routes, auth and entitlement evidence before registration.
- **Kimi Code:** Use the region-matched Kimi Code subscription key, not a Moonshot Open Platform key. Pave sends the truthful `User-Agent: Pave`; Kimi's [official API guide](https://www.kimi.com/code/docs/) requires the client identity not be impersonated. The route does not establish plan entitlement.
- **Alibaba Token Plan:** The `sk-sp-` route is distinct from Alibaba Coding Plan and the workspace API. Confirm the current Token Plan terms and supported-tool eligibility; a Pave route is not proof of account access or authorization.
- **Umans:** `/v1/models/info` reports the provider's current model/capability data. The listing does not validate the configured key or prove account access; prices remain unknown in Pave.
- **Anthropic prompt cache:** Only the direct `https://api.anthropic.com/v1/messages` API-key route opts into Anthropic's automatic ephemeral prefix cache; OAuth and custom/compatible endpoints do not. The default cache lifetime is five minutes. Cache writes can have different billing, and cache retention terms may differ; check current Anthropic pricing and data-retention terms. Pave records provider-reported cache read/write tokens and does not estimate dollars.

**Coverage and limits**

- Linux/macOS Intel builds list 71 provider IDs across 70 of the 83 source identities (13 unmatched); macOS arm64 adds Apple Foundation Models when its helper is present (72 IDs, 71 source identities, 12 unmatched). The first 15 routes added after v0.1.39 passed native fake-HTTPS `read_file` turns. R3 route, discovery, reasoning-replay and CLI tool-result fixtures passed; fixtures do not prove live entitlement.
- Command Code catalogs describe endpoints but cannot validate a Studio key: choose `--api`. GitLab Duo has no authoritative non-agentic upstream-model roster: supply route and model. Unverified listings stay unclassified in `/model`.
- Alibaba Coding Plan requires an explicit `--api china|intl`; its key is distinct from the Beijing Token Plan key. Xiaomi Token Plan environment keys and endpoints are region-bound, unlike Xiaomi pay-as-you-go. Cloudflare requires a configured account/gateway and uses the gateway-auth header; none of these routes has a Pave account-model listing.
- Cursor's official SDK bridge uses Connect over HTTP/1.1; `SdkCursorService.ListModels` is separate from its agent-run lifecycle, so Cursor has no generic Pave completion route. GitLab Duo Agent's persistent WebSocket/runtime remains unsupported; GitLab Duo Direct Access is a separate supported route. Gemini CLI, Kimi Code device OAuth, Muse, Stencil/Z.AI Coding Plan, xAI subscription OAuth, Perplexity and Copilot Enterprise also remain unavailable pending the documented matching registration/route/transport prerequisites. Google's [Antigravity terms](https://antigravity.google/terms/) prohibit third-party OAuth clients; standard API keys are not mislabeled as subscription grants.
