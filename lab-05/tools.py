import torch
import torch.nn as nn
import torch.optim as optim
import pandas as pd
import numpy as np
from PIL import Image, ImageFilter
from facenet_pytorch import InceptionResnetV1, MTCNN
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score
import matplotlib.pyplot as plt
import random

if not torch.cuda.is_available():
    print("CUDA is not available, using CPU")
    raise RuntimeError("CUDA is not available")

device = torch.device("cuda")
mtcnn = MTCNN(image_size=160, margin=20, device=device)
facenet = InceptionResnetV1(pretrained="vggface2").eval().to(device)

seed = 42
random.seed(seed)
np.random.seed(seed)
torch.manual_seed(seed)


class FaceVerificationMLP(nn.Module):
    def __init__(self, input_dim=512):
        super(FaceVerificationMLP, self).__init__()
        self.model = nn.Sequential(
            nn.Linear(input_dim, 256),
            nn.ReLU(),
            nn.Linear(256, 64),
            nn.ReLU(),
            nn.Linear(64, 2),
        )

    def forward(self, x):
        return self.model(x)


def split(df: pd.DataFrame, train_size: int, test_size: int) -> tuple[list, list]:
    df = df.sample(frac=1, random_state=seed).reset_index(drop=True)

    test = df.iloc[:test_size]
    forbidden = set(test["file1"]) | set(test["file2"])

    remaining = df.iloc[test_size:]
    filtered = remaining[
        (~remaining["file1"].isin(forbidden)) & (~remaining["file2"].isin(forbidden))
    ].reset_index(drop=True)

    train = filtered.iloc[:train_size]

    return (
        list(train.itertuples(index=False, name=None)),
        list(test.itertuples(index=False, name=None)),
    )


def embedding(image_path, perturbation: bool) -> torch.Tensor:
    img = Image.open(image_path).convert("RGB")

    if perturbation:
        img = img.filter(ImageFilter.GaussianBlur(radius=2))

    face = mtcnn(img)
    if face is None:
        raise ValueError(f"No face detected in {image_path}")

    face = face.unsqueeze(0).to(device)
    embedding = facenet(face)

    return embedding.detach().to(device)


def diff_vector(img1, img2, perturbation: bool = False) -> torch.Tensor:
    e1 = embedding(f"archive/images/{img1}", perturbation)
    e2 = embedding(f"archive/images/{img2}", perturbation)
    return torch.abs(e1 - e2)


def prepare(pairs, perturbation: bool = False) -> tuple[torch.Tensor, torch.Tensor]:
    X, y = [], []

    for img1, img2, label in pairs:
        try:
            diff = diff_vector(img1, img2, perturbation).to(device)
            X.append(diff.squeeze(0))
            y.append(label)
        except Exception:
            print("Error preparing")

    X_tensor = torch.stack(X).to(device)
    y_tensor = torch.tensor(y, device=device)

    return X_tensor, y_tensor


def train(X_train, y_train, lr=0.0001, epochs=100) -> nn.Module:
    model = FaceVerificationMLP().to(device)
    optimizer = optim.Adam(model.parameters(), lr=lr)
    criterion = nn.CrossEntropyLoss()

    X_train = X_train.to(device)
    y_train = y_train.to(device)

    for epoch in range(epochs):
        model.train()
        optimizer.zero_grad()
        outputs = model(X_train)
        loss = criterion(outputs, y_train)
        loss.backward()
        optimizer.step()

    return model


def train_with_adaptive_learning_rate(
    X_train, y_train, base_lr=0.001, epochs=100
) -> nn.Module:
    model = FaceVerificationMLP().to(device)
    optimizer = optim.Adam(model.parameters(), lr=base_lr)
    scheduler = optim.lr_scheduler.ReduceLROnPlateau(
        optimizer, mode="min", factor=0.5, patience=5, verbose=True
    )
    criterion = nn.CrossEntropyLoss()

    X_train = X_train.to(device)
    y_train = y_train.to(device)

    for epoch in range(epochs):
        model.train()
        optimizer.zero_grad()
        outputs = model(X_train)
        loss = criterion(outputs, y_train)
        loss.backward()
        optimizer.step()
        scheduler.step(loss)

    return model


def evaluate(model, test_data) -> tuple[float, float, float, float]:
    model.eval()
    model = model.to(device)
    y_true, y_pred = [], []

    for img1, img2, label in test_data:
        try:
            diff = diff_vector(img1, img2)
            output = model(diff.to(device))
            _, predicted = torch.max(output, 1)
            y_pred.append(predicted.item())
            y_true.append(label)
        except Exception:
            print("Error predicting")

    return (
        accuracy_score(y_true, y_pred),
        precision_score(y_true, y_pred),
        recall_score(y_true, y_pred),
        f1_score(y_true, y_pred),
    )


def plot(metric_values, x_values, x_label, title_prefix) -> None:
    plt.figure(figsize=(12, 6))
    metrics = ["Accuracy", "Precision", "Recall", "F1-score"]
    for i, metric in enumerate(metrics):
        plt.plot(x_values, [v[i] for v in metric_values], label=metric)
    plt.xlabel(x_label)
    plt.ylabel("Score")
    plt.title(f"{title_prefix} vs {x_label}")
    plt.legend()
    plt.grid(True)
    plt.show()
